module Penna
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Penna
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing one of the core procedures for the simulation of the
  !!  Penna model (along with the `PopulationList` module)
  ! -------------------------------------------------------------------------- !
  use Parameters, only:            &
    MODEL_L,                       &
    MODEL_B,                       &
    MODEL_M,                       &
    MODEL_R,                       &
    MODEL_R_MAX,                   &
    MODEL_T,                       &
    MODEL_K,                       &
    MODEL_TIME_STEPS,              &
    MODEL_MTTN_COUNT,              &
    MODEL_START_POP_SIZE,          &
    MODEL_ENTROPY_ORDER,           &
    MODEL_AGE_DSTRB_INIT_TIMESTEP, &
    PROG_REC_FLAG,                 &
    PROG_IN_CSV_FMT,               &
    PROG_SAMPLE_SIZE,              &
    PROG_PRINT_STATE,              &
    PROG_RNG,                      &
    PROG_RNG_SEED,                 &
    PROG_OUT_FILE_NAME,            &
    REC_NULL,                      &
    REC_POP,                       &
    REC_AGE_DSTRB,                 &
    REC_DEATH,                     &
    REC_DIV_IDX,                   &
    REC_GENE_DSTRB,                &
    REC_TIME,                      &
    REC_GNM_COUNT,                 &
    SILENT_PRINT,                  &
    setParams,                     &
    printProgDetails,              &
    freeParamAlloctbls

  use Demographics, only: &
    ageDistribution,      &
    resetAgeDstrb,        &
    updateAgeDstrb,       &
    deallocAgeDstrb,      &
    initGenomeDstrb,      &
    freeGenomeDstrb,      &
    getDiversityIdx,      &
    getBadGeneDstrb,      &
    getUniqueGenomeCount

  use PopulationList, only: &
    ALIVE,                  &
    DEAD_OLD_AGE,           &
    DEAD_MUTATION,          &
    DEAD_VERHULST,          &
    Population_t,           &
    Person_t
  
  use DataWriter, only:  &
    getWriterPtr,        &
    isWriterInitialized, &
    initDataWriter,      &
    Writer,              &
    writeIK,             &
    writeRK

  use, intrinsic :: iso_fortran_env, only: &
    timeIK => int64, &
    timeRK => real64

  use CastProcs, only: castIntToChar
  use ProgressBarType, only: ProgressBar
  use RandNumProcs, only: assignRNGParams
  use ErrorMSG, only: raiseError, raiseWarning
  implicit none
  private

  ! Header divider for external files to be written on.
  character(len=*), parameter :: FILE_DIVIDER = "---------------"

  public :: run
  public :: initProgram
  public :: freeAlloctbls
  public :: printProgDetails
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initProgram
  !>  A wrapper subroutine to initialize various module variables.
  ! -------------------------------------------------------------------------- !
  subroutine initProgram()
    ! Get the model and program parameters.
    call setParams()

    ! Get the chosen RNG and RNG seed.
    call assignRNGParams(PROG_RNG, PROG_RNG_SEED)
  end subroutine initProgram


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeAlloctbls
  !>  A wrapper subroutine to deallocate any allocatable variables in other
  !!  module.
  ! -------------------------------------------------------------------------- !
  subroutine freeAlloctbls()
    call deallocAgeDstrb()
    call freeParamAlloctbls()
  end subroutine freeAlloctbls


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: runOneInstance
  !>  Run the Penna model simulation once.
  ! -------------------------------------------------------------------------- !
  subroutine runOneInstance(maxTimestep, startPopSize, initMttnCount,recordFlag)
    integer,          intent(in) :: maxTimestep
      !! Maximum (total) time step.
    integer,          intent(in) :: startPopSize
      !! Starting population size.
    integer,          intent(in) :: initMttnCount
      !! Initial mutation count of each individuals.
    character(len=*), intent(in) :: recordFlag
      !! Record flag. Can have multiple values.

    type(Population_t)      :: population
    type(Person_t), pointer :: currPersonPtr

    integer, target :: deathCount(3) ! Death count 
    integer :: timeStep              ! Time step
    integer :: popSize               ! Population size
    
    integer :: recordFlagLen         ! Number of record flags.
    logical :: recordGnmDstrb
    logical :: recordDeath
    logical :: recordAgeDstrb

    ! True if the time step is within the range for recording age demographics
    logical :: withinAgeDemogRange

    integer, pointer :: deathByAge
    integer, pointer :: deathByMutation
    integer, pointer :: deathByVerhulst

    ! === MODIFICATION ADDED FOR VARYING PARAMETERS ===
    ! integer, pointer :: paramToChange
    ! integer, pointer :: paramToChange2
    ! ================================================= !

    ! Initialize data writers
    recordFlagLen = len(recordFlag)

    ! Initialize inquiry flags for data recording.
    recordGnmDstrb = &
        isWriterInitialized(REC_DIV_IDX // REC_GENE_DSTRB // REC_GNM_COUNT)
    recordDeath    = isWriterInitialized(REC_DEATH)
    recordAgeDstrb = isWriterInitialized(REC_AGE_DSTRB)
    withinAgeDemogRange = (maxTimestep <= MODEL_AGE_DSTRB_INIT_TIMESTEP)
      
    ! Initialize the death counters.
    deathCount(:) = 0
    deathByAge      => deathCount(1)
    deathByMutation => deathCount(2)
    deathByVerhulst => deathCount(3)

    ! Initialize the population
    popSize = startPopSize
    call initGenomeDstrb()
    call resetAgeDstrb()
    population = Population_t(startPopSize, initMttnCount, recordGnmDstrb)

    ! === MODIFICATION ADDED FOR VARYING PARAMETERS === !
    ! paramToChange  => !!!!!!
    ! ================================================= !
    
    ! Record data of the initial state of the population.
    ! The data that would be obtained at this point in the program
    ! represent the data at t = 0.
    call recordData()

    ! Run the model.
    mainLoop: do timeStep = 1, maxTimestep
      ! Catch case when the population size exceeds the carrying capacity.
      ! NOTE:
      !   With Verhulst death enabled, this does not happen. But if it is
      !   disabled, there is no mechanism to prevent runaway population growth.
      !   In this Penna model program, the carrying capacity acts as a 
      !   "fail-safe" to prevent any unintended runaway growth.
      runawayGrowthCheck: if (popSize > MODEL_K) then
        call raiseWarning(                                           &
            "The population has exceeded the carrying capacity! " // &
            "Stopping the current run."                              &
          )
        exit mainLoop
      end if runawayGrowthCheck

      ! Age demogaphics range check.
      withinAgeDemogRange = (                                             &
              (maxTimestep - timeStep) <= MODEL_AGE_DSTRB_INIT_TIMESTEP   &
        )

      ! === MODIFICATION ADDED FOR VARYING PARAMETERS === !
      ! if (modulo(popSize, 200) == 0) then
      !   paramToChange  = paramToChange  + 1
      !   paramToChange2 = paramToChange2 + 1
      ! end if
      ! ================================================= !

      ! Ready the population for evaluation in the current time step.
      call population%startCurrStep()

      evalPop: do while(.not. population%atEndOfPopulation())
        ! Evaluate the current person for death and birth events.
        call population%evalCurrPerson()

        ! Access the current person to record the requested data.
        currPersonPtr => population%getCurrPerson()
        if (.not.associated(currPersonPtr)) then
          call raiseError("Internal error. Null current `Person_t` pointer.")
        end if

        ! === DATA RECORDING FOR LIVE INDIVIDUALS === !
        postEvalDataRec: if (currPersonPtr%lifeStat == ALIVE) then
          ! --- Data recording: Age demographics
          if (recordAgeDstrb .and. withinAgeDemogRange) then
            call updateAgeDstrb(currPersonPtr%age)
          end if
        ! === DATA RECORDING FOR LIVE INDIVIDUALS === !
        else if (recordDeath) then
            ! --- Data recording: Death count
            select case(currPersonPtr%lifeStat)
              case(DEAD_OLD_AGE);  deathByAge      = deathByAge      + 1
              case(DEAD_MUTATION); deathByMutation = deathByMutation + 1
              case(DEAD_VERHULST); deathByVerhulst = deathByVerhulst + 1
              case default
                call raiseError("Internal error encountered. Invalid lifeStat.")
            end select
        end if postEvalDataRec

        ! Go to the next person.
        call population%next()
      end do evalPop
      
      ! Finalize the population evaluation
      call population%endCurrStep()
      popSize = population%getPopSize()

      ! Record data.
      call recordData()
      ! Reset counters.
      if (recordDeath)    deathCount(:) = 0
      if (recordAgeDstrb) call resetAgeDstrb()
    end do mainLoop

    ! Clean up any allocated objects.
    call freeGenomeDstrb()
    call population%cleanup()
  contains


    ! ------------------------------------------------------------------------ !
    ! `runOneInstance` SUBROUTINE: recordData
    !>  Record data obtained in the subroutine `runOneInstance`.
    ! ------------------------------------------------------------------------ !
    subroutine recordData()
      type(Writer), pointer :: chosenWriter
      integer :: i

      do i = 1, recordFlagLen
        chosenWriter => getWriterPtr(recordFlag(i: i))

        ! Write data into a file as specified by `charFlag`.
        select case (recordFlag(i: i))
          case (REC_POP)
            call chosenWriter%write(int(popSize, kind=writeIK))

          case (REC_AGE_DSTRB)
            if (withinAgeDemogRange) then
              call chosenWriter%write(int(ageDistribution, kind=writeIK))
            end if

          case (REC_DEATH)
            call chosenWriter%write(int(deathCount, kind=writeIK))

          case (REC_DIV_IDX)
            call chosenWriter%write( &
                real(getDiversityIdx(MODEL_ENTROPY_ORDER), kind=writeRK) &
            )
          
          case (REC_GENE_DSTRB)
            call chosenWriter%write(int(getBadGeneDstrb(), kind=writeIK))

          case (REC_GNM_COUNT)
            call chosenWriter%write(int(getUniqueGenomeCount(), kind=writeIK))
        end select
      end do
    end subroutine recordData
  end subroutine runOneInstance


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: run
  !>  Run the Penna model simulation. This is a wrapper subroutine to the
  !!  subroutine `runMultipleInstance`.
  ! -------------------------------------------------------------------------- !
  subroutine run()
    real(kind=timeRK)    :: meanTime
    real(kind=timeRK)    :: stdDevTime
    integer(kind=timeIK) :: startTimeInt
    integer(kind=timeIK) :: endTimeInt
    real(kind=timeRK)    :: startTimeReal
    real(kind=timeRK)    :: endTimeReal
    real(kind=timeRK)    :: clockRate
    real(kind=timeRK)    :: sum
    real(kind=timeRK)    :: sumSqrd

    type(Writer), pointer :: timeWriter ! A `Writer` object for writing timings.
    type(ProgressBar)     :: progBar    ! A `ProgressBar` object.
    logical :: printProgress
    integer :: i

    ! Print separator for pretty printing.
    character, parameter :: PRINT_SEPARATOR(*) = [("=", i = 1, 29)]

    ! Shorthand form
    printProgress = PROG_PRINT_STATE /= SILENT_PRINT

    ! Initialize the progress bar.
    progBar = ProgressBar(20, PROG_SAMPLE_SIZE)

    ! Initialize data output writing.
    call initDataWriter(trim(PROG_REC_FLAG),PROG_OUT_FILE_NAME, PROG_IN_CSV_FMT)

    ! Call and time the `run` subroutine.
    sum = 0._timeRK
    sumSqrd = 0._timeRK
    do i = 1, PROG_SAMPLE_SIZE
      ! Start timer.
      call system_clock(count=startTimeInt, count_rate=clockRate)  
      startTimeReal = real(startTimeInt, kind=timeRK)/clockRate

      ! Run the actual simulation once.
      call runOneInstance(                   &
          maxTimeStep=MODEL_TIME_STEPS,      &
          startPopSize=MODEL_START_POP_SIZE, &
          initMttnCount=MODEL_MTTN_COUNT,    &
          recordFlag=trim(PROG_REC_FLAG)     &
        )

      ! End timer.
      call system_clock(count=endTimeInt, count_rate=clockRate)
      endTimeReal = real(endTimeInt, kind=timeRK)/clockRate

      ! Calculate necessary values for average and std deviation.
      sum = sum + (endTimeReal - startTimeReal)*1e3
      sumSqrd = sumSqrd + ((endTimeReal - startTimeReal)*1e3)**2

      ! Print the progress bar.
      if (printProgress .and. PROG_SAMPLE_SIZE > 1) then
        call progBar % incrCounter(show=.true.)
      end if
    end do

    ! Remove the progress bar.
    write(*, "(a)", advance="no") char(13)

    ! Get average elapsed time and its std deviation.
    meanTime = sum/real(PROG_SAMPLE_SIZE, kind=timeRK)
    stdDevTime = sqrt(PROG_SAMPLE_SIZE*sumSqrd - sum**2) / &
      real(PROG_SAMPLE_SIZE, kind=timeRK)

    ! Print timing statistics.
    if (printProgress) then
      ! Print elapsed time.
      if (PROG_SAMPLE_SIZE > 1) then
        print "(a, f12.3, a)", "Average time: ", meanTime, " ms"
      else
        print "(a, f12.3, a)", "Elapsed time: ", meanTime, " ms"
      end if

      ! Print the standard deviation.
      if (PROG_SAMPLE_SIZE > 1) &
        print "(a, f11.3, a)", "Std deviation: ", stdDevTime, " ms"
    end if

    ! Record mean time and std deviation.
    if (isWriterInitialized(REC_TIME)) then
      timeWriter => getWriterPtr(REC_TIME)

      ! Write the actual timing statistics.
      call timeWriter%write([                       &
          real(MODEL_TIME_STEPS, kind=writeRK),     &
          real(MODEL_START_POP_SIZE, kind=writeRK), &
          real(PROG_SAMPLE_SIZE, kind=writeRK),     &
          real(meanTime, kind=writeRK),             &
          real(stdDevTime, kind=writeRK)]           &
        )
    end if

    if (printProgress) print "(*(a))", PRINT_SEPARATOR
  end subroutine run
end module Penna

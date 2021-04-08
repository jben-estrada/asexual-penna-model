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
  use Parameters, only:   &
    MODEL_L,              &
    MODEL_B,              &
    MODEL_K,              &
    MODEL_TIME_STEPS,     &
    MODEL_MTTN_COUNT,     &
    MODEL_START_POP_SIZE, &
    PROG_REC_FLAG,        &
    PROG_IN_CSV_FMT,      &
    PROG_SAMPLE_SIZE,     &
    PROG_PRINT_STATE,     &
    PROG_RNG,             &
    PROG_RNG_SEED,        &
    PROG_OUT_FILE_NAME,   &
    SILENT_PRINT,         &
    setParams,            &
    printProgDetails,     &
    freeParamAlloctbls

  use Demographics, only: &
    ageDistribution,      &
    DEF_DEMOG_LAST_STEP,  &
    DEMOG_LAST_STEPS,     &
    resetAgeDstrb,        &
    updateAgeDstrb,       &
    deallocAgeDstrb,      &
    updateGenomeDstrb,    &
    freeGenomeDstrbList,  &
    getDiversityIdx,      &
    getBadGeneDstrb

  use PopulationList, only: &
    ALIVE,                  &
    DEAD_OLD_AGE,           &
    DEAD_MUTATION,          &
    DEAD_VERHULST,          &
    Population_t,           &
    defaultPersonPtr,       &
    Person_t
  
  use DataWriter, only:  &
    REC_NULL,            &
    REC_POP,             &
    REC_AGE_DSTRB,       &
    REC_DEATH,           &
    REC_DIV_IDX,         &
    REC_GENE_DSTRB,      &
    REC_TIME,            &
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

    type(Population_t) :: population

    integer, target :: deathCount(3) ! Death count 
    integer :: timeStep               ! Time step
    integer :: popSize                ! Population size

    integer, pointer :: deathByAge
    integer, pointer :: deathByMutation
    integer, pointer :: deathByVerhulst

    ! Initialization
    popSize = startPopSize
    deathCount(:) = 0
    call resetAgeDstrb(MODEL_L)
    population = Population_t(startPopSize, initMttnCount)

    ! Initialize pointers.
    deathByAge => deathCount(1)
    deathByMutation => deathCount(2)
    deathByVerhulst => deathCount(3)

    ! Enable/disable demographics recording.
    if (isWriterInitialized(REC_AGE_DSTRB)) then
      DEMOG_LAST_STEPS = DEF_DEMOG_LAST_STEP
    else
      DEMOG_LAST_STEPS = -1
    end if

    ! Record data of the initial state of the population.
    ! The data that would be obtained at this point in the program
    ! represent the data at t = 0.
    call recordData()

    ! Run the model.
    mainLoop: do timeStep = 1, maxTimestep
      ! Catch case when pop size exceeds the carrying capacity.
      ! For this given set of model parameters, pop size might explode.
      ! As such, we halt the run once this case is reached.
      if (popSize > MODEL_K) then
        call raiseWarning("The population has exceeded the carrying" // &
        " capacity! Stopping the current run.")
        exit
      end if

      ! Evaluate population.
      call evalPopulation(population, maxTimestep - timeStep, &
          deathByAge, deathByMutation, deathByVerhulst)
      popSize = population%getPopSize()

      ! Record data.
      call recordData()

      ! Reset counters.
      select case(recordFlag)
        case(REC_DEATH)
          deathCount(:) = 0
        case(REC_AGE_DSTRB)
          call resetAgeDstrb(MODEL_L)
        case(REC_DIV_IDX, REC_GENE_DSTRB)
          call freeGenomeDstrbList()
      end select
    end do mainLoop

    ! Wrap up.
    call population%cleanup()
  contains


    ! ------------------------------------------------------------------------ !
    ! SUBROUTINE: recordData
    !>  Record data obtained in the subroutine `runOneInstance`. This is a
    !!  subroutine only within the scope of `runOneInstance`.
    ! ------------------------------------------------------------------------ !
    subroutine recordData()
      type(Writer), pointer :: chosenWriter
      integer :: i

      do i = 1, len(recordFlag)
        chosenWriter => getWriterPtr(recordFlag(i: i))

        ! Write data into a file as specified by `charFlag`.
        select case (recordFlag(i: i))
          case (REC_POP)
            call chosenWriter%write(int(popSize, kind=writeIK))

          case (REC_AGE_DSTRB)
            call chosenWriter%write(int(ageDistribution, kind=writeIK))

          case (REC_DEATH)
            call chosenWriter%write(int(deathCount, kind=writeIK))

          case (REC_DIV_IDX)
            call chosenWriter%write(real(getDiversityIdx(), kind=writeRK))
          
          case (REC_GENE_DSTRB)
            call chosenWriter%write(int(getBadGeneDstrb(), kind=writeIK))
        end select
      end do
    end subroutine recordData
  end subroutine runOneInstance


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalPopulation
  !>  Evaluate the population for one time step.
  ! -------------------------------------------------------------------------- !
  subroutine evalPopulation( &
      population,       &
      countdown,        &
      deathByAge,       &
      deathByMutation,  &
      deathByVerhulst   &
     )
    ! use Gene, only: personIK
    type(Population_t), intent(inout) :: population
    integer, pointer,   intent(inout) :: deathByAge       !! Death by age count
    integer, pointer,   intent(inout) :: deathByMutation  !! Death by mutation
    integer, pointer,   intent(inout) :: deathByVerhulst  !! Random death
    integer,            intent(in)    :: countdown        !! Count from max time

    type(Person_t), pointer :: currPerson

    evalPop: do while(.not. population%atEndOfPopulation())
      ! Evaluate the current person. If this person is alive, its age is
      ! incremented and birth event is checked.
      call population%evalCurrPerson(isWriterInitialized(REC_GENE_DSTRB))

      currPerson => defaultPersonPtr(population%getCurrPerson())
      if (.not.associated(currPerson)) then
        call raiseError("Internal error. Null current `Person_t` pointer.")
      end if

      if (currPerson%lifeStat == ALIVE) then
        ! Update the genome distribution.
        if (isWriterInitialized(REC_DIV_IDX) .or. &
            isWriterInitialized(REC_GENE_DSTRB)) then
           call updateGenomeDstrb(currPerson%genome)
        end if
      else
        if (isWriterInitialized(REC_DEATH)) then
          ! Increment the appropriate death counter.
          select case(currPerson%lifeStat)
            case(DEAD_OLD_AGE);  deathByAge = deathByAge + 1
            case(DEAD_MUTATION); deathByMutation = deathByMutation + 1
            case(DEAD_VERHULST); deathByVerhulst = deathByVerhulst + 1
            case default
              call raiseError("Internal error encountered. Invalid lifeStat.")
          end select
        end if
      end if

      ! Record age demographics.
      if (countdown <= DEMOG_LAST_STEPS) then
        call updateAgeDstrb(currPerson%age, ageDistribution)
      end if

      ! Go to the next person.
      call population%next()
    end do evalPop

    call population%endCurrStep()
  end subroutine evalPopulation


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

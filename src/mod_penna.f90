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
    MODEL_TIME_DEPENDENT_PARAM_PTR,&
    PROG_REC_FLAG,                 &
    PROG_SAMPLE_SIZE,              &
    PROG_PRINT_STATE,              &
    PROG_RNG,                      &
    PROG_RNG_SEED,                 &
    PROG_OUT_FILE_NAME,            &
    PROG_OUT_FMT,                  &
    TERM_OUT_WIDTH,                &
    REC_NULL,                      &
    REC_POP,                       &
    REC_AGE_DSTRB,                 &
    REC_DEATH,                     &
    REC_DIV_IDX,                   &
    REC_GENE_DSTRB,                &
    REC_TIME,                      &
    REC_GNM_COUNT,                 &
    NORMAL_PRINT,                  &
    SILENT_PRINT,                  &
    setParams,                     &
    incrementTimeDependentParam,   &
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
    init_Population,        &
    Person_t
  
  use DataWriter, only:  &
    getWriterPtr,        &
    isWriterInitialized, &
    initDataWriter,      &
    closeDataWriter,     &
    Writer_t,            &
    writeIK,             &
    writeRK,             &
    DATA_WRITER_PENNA,   &
    DATA_WRITER_PROG

  use, intrinsic :: iso_fortran_env, only: &
    timeIK => int64,  &
    timeRK => real64

  use CastProcs, only: castIntToChar
  use RandNumProcs, only: assignRNGParams, setSeed
  use ErrorMSG, only: raiseError, raiseWarning
  use ProgressBarType, only: ProgressBar_t, init_ProgressBar
  implicit none
  private

  ! Header divider for external files to be written on.
  character(len=*), parameter :: FILE_DIVIDER = "---------------"

  logical :: toRecPop       = .false.
  logical :: toRecDeath     = .false.
  logical :: toRecDivIdx    = .false.
  logical :: toRecGeneDstrb = .false.
  logical :: toRecGnmCount  = .false.
  logical :: toRecAgeDstrb  = .false.
  logical :: toRecTime      = .false.

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

    ! True if the time step is within the range for recording age demographics
    logical :: withinAgeDemogRange
    ! True if a model parameter is set to change with time.
    logical :: incrementingModelParam

    integer, pointer :: deathByAge
    integer, pointer :: deathByMutation
    integer, pointer :: deathByVerhulst

    ! Initialize data writers
    recordFlagLen = len(recordFlag)

    ! Initialize the inquiry flags.
    withinAgeDemogRange = (maxTimestep <= MODEL_AGE_DSTRB_INIT_TIMESTEP)
    incrementingModelParam = associated(MODEL_TIME_DEPENDENT_PARAM_PTR)
      
    ! Initialize the death counters.
    deathCount(:) = 0
    deathByAge      => deathCount(1)
    deathByMutation => deathCount(2)
    deathByVerhulst => deathCount(3)

    ! Initialize the population
    popSize = startPopSize
    call initGenomeDstrb()
    call resetAgeDstrb()
    call init_Population( &
        population,         &
        startPopSize,       &
        initMttnCount,      &
        toRecDivIdx .or. toRecGeneDstrb .or. toRecGnmCount &
      )

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

      ! Check if the time-dependent model parameter is set to change at the
      ! current time step.
      if (incrementingModelParam) then
        call incrementTimeDependentParam(timeStep)
      end if

      ! Age demogaphics range check for the current time step.
      withinAgeDemogRange = (                                             &
              (maxTimestep - timeStep) <= MODEL_AGE_DSTRB_INIT_TIMESTEP   &
        )

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
          if (toRecAgeDstrb .and. withinAgeDemogRange) then
            call updateAgeDstrb(currPersonPtr%age)
          end if
        ! === DATA RECORDING FOR LIVE INDIVIDUALS === !
        else if (toRecDeath) then
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
      if (toRecDeath)    deathCount(:) = 0
      if (toRecAgeDstrb) call resetAgeDstrb()
    end do mainLoop

    ! Reset the time-dependent model parameter to its starting value.
    if (incrementingModelParam) then
      call incrementTimeDependentParam(0, reset=.true.)
    end if

    ! Clean up any allocated objects.
    call freeGenomeDstrb()
    call population%cleanup()
  contains


    ! ------------------------------------------------------------------------ !
    ! `runOneInstance` SUBROUTINE: recordData
    !>  Record data obtained in the subroutine `runOneInstance`.
    ! ------------------------------------------------------------------------ !
    subroutine recordData()
      type(Writer_t), pointer :: chosenWriter
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
  ! SUBROUTINE: setWriterInqFlag
  !>  Set the global statuses of the data writers based on the user's inputs
  ! -------------------------------------------------------------------------- !
  subroutine setWriterInqFlag(recordFlags)
    character(len=*), intent(in) :: recordFlags
    integer :: i

    do i = 1, len(recordFlags)
      select case (recordFlags(i:i))
      case (REC_POP)
        ToRecPop = .true.
      case (REC_DEATH)
        ToRecDeath = .true.
      case (REC_DIV_IDX)
        ToRecDivIdx = .true.
      case (REC_GENE_DSTRB)
        ToRecGeneDstrb = .true.
      case (REC_GNM_COUNT)
        ToRecGnmCount = .true.
      case (REC_AGE_DSTRB)
        ToRecAgeDstrb = .true.
      case (REC_TIME)
        ToRecTime = .true.
      end select
    end do
  end subroutine setWriterInqFlag


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: run
  !>  Run the Penna model simulation.
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

    type(Writer_t), pointer :: timeWriter ! Object for writing the timings.
    type(ProgressBar_t)     :: progBar    ! A progress bar object.

    ! Print separator for pretty printing.
    character(len=*), parameter :: PRINT_SEPARATOR = repeat("=", TERM_OUT_WIDTH)

    integer, parameter :: T_UNIT_LEN = 2
    integer, parameter :: T_STR_LEN  = 8

    character(len=T_UNIT_LEN), parameter :: TIMING_UNIT_MS  = "ms"
    character(len=T_UNIT_LEN), parameter :: TIMING_UNIT_S   = "s "
    integer, parameter :: TIMING_TIME_DESC_LEN = 20 - (T_UNIT_LEN + 1)
    integer, parameter :: TIMING_TIME_VAL_LEN  = 10

    character(len=T_STR_LEN) :: timing_desc_fmt
    character(len=T_STR_LEN) :: timing_val_fmt
    character(len=T_STR_LEN) :: timing_unit
    logical :: printProgress
    integer :: i

    ! Shorthand form
    printProgress = PROG_PRINT_STATE /= SILENT_PRINT

    timing_unit = TIMING_UNIT_MS
    timing_desc_fmt = "a" // castIntToChar(TIMING_TIME_DESC_LEN)
    timing_val_fmt  = "f" // castIntToChar(TIMING_TIME_VAL_LEN) // ".3"

    ! Initialize the progress bar.
    call init_ProgressBar(progBar, PROG_SAMPLE_SIZE, totalBarLen=TERM_OUT_WIDTH)

    ! Set the global inquiry flags for data to be recorded.
    call setWriterInqFlag(trim(PROG_REC_FLAG))

    ! Initialize data writing for the current program run (e.g. time elapsed)
    call initDataWriter(       &
          trim(PROG_REC_FLAG), &
          PROG_OUT_FILE_NAME,  &
          PROG_OUT_FMT,        &
          DATA_WRITER_PROG,    &
          1                    &   ! Only one set of data for run analyses
        )
    
    ! Initialize timing statistics.
    sum = 0._timeRK
    sumSqrd = 0._timeRK
        
    ! Show the initial progress bar.
    if (printProgress) call progBar % showProgBar()
    ! Call and time the `run` subroutine.
    do i = 1, PROG_SAMPLE_SIZE
      ! Initialize data writing for Penna data.
      call initDataWriter(     &
          trim(PROG_REC_FLAG), &
          PROG_OUT_FILE_NAME,  &
          PROG_OUT_FMT,        &
          DATA_WRITER_PENNA,   &
          i                    &
        )

      ! Start timer.
      call system_clock(count=startTimeInt, count_rate=clockRate)  
      startTimeReal = real(startTimeInt, kind=timeRK)/clockRate

      ! Run the Penna model simulation once.
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

      ! Finalize and close the Penna data writers.
      call closeDataWriter(DATA_WRITER_PENNA)

      ! Reset the RNG with a new seed.
      call setSeed(PROG_RNG_SEED + i)

      ! Print the progress bar.
      if (printProgress) then
        call progBar % incrCounter(show=.true.)
      end if
    end do

    ! Get average elapsed time and its std deviation.
    meanTime = sum/real(PROG_SAMPLE_SIZE, kind=timeRK)
    stdDevTime = sqrt(PROG_SAMPLE_SIZE*sumSqrd - sum**2) / &
      real(PROG_SAMPLE_SIZE, kind=timeRK)

    ! Print timing statistics.
    if (printProgress) then
      ! Remove the progress bar.
      if (PROG_PRINT_STATE == NORMAL_PRINT) then
        call progBar % clear()
      end if

      ! Turn milliseconds to seconds if the elapsed time is too large to read
      if (meanTime > 0.0) then
        if (floor(log10(meanTime)) + 1 >= 4) then
          meanTime   = meanTime / 1000.0   ! From milliseconds to seconds
          stdDevTime = stdDevTime / 1000.0
          timing_unit = TIMING_UNIT_S
        end if
      end if

      ! Print elapsed time.
      if (PROG_SAMPLE_SIZE > 1) then
        print "(/" // timing_desc_fmt // "," // timing_val_fmt // ", x, a2)", &
            "Average time:", meanTime, timing_unit
        print "(" // timing_desc_fmt // "," // timing_val_fmt // ", x, a2)",  &
            "Std deviation:", stdDevTime, timing_unit
      else
        print "(/" // timing_desc_fmt // "," // timing_val_fmt // ", x, a2)", &
            "Elapsed time: ", meanTime, timing_unit
      end if
    end if

    ! Record mean time and std deviation.
    if (toRecTime) then
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

    ! Close the other data writers.
    call closeDataWriter(DATA_WRITER_PROG)

    if (printProgress) print "(a)", PRINT_SEPARATOR
  end subroutine run
end module Penna

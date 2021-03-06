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
    PROG_SAMPLE_SIZE,     &
    PROG_RECORD_TIME,     &
    PROG_PRINT_STATE,     &
    PROG_RNG,             &
    PROG_RNG_SEED,        &
    PROG_OUT_FILE_NAME,   &
    PROG_TIME_FILE_NAME,  &
    SILENT_PRINT,         &
    REC_NULL,             &
    REC_POP,              &
    REC_AGE_DSTRB,        &
    REC_DEATH,            &
    REC_DIV_IDX,          &
    REC_GENE_DSTRB,       &
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

  use, intrinsic :: iso_fortran_env, only: &
    timeIK => int64, &
    timeRK => real64

  use CastProcs, only: castIntToChar
  use ProgressBarType, only: ProgressBar
  use RandNumProcs, only: assignRNGParams
  use ErrorMSG, only: raiseError, raiseWarning
  use WriterType, only: Writer, writeIK, writeRK
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
      !! Record flag. Valid values are found in the `WriterOptions` module.

    type(Population_t) :: population

    type(Writer)     :: runWriter     ! `Writer` object for recording data 
    integer, target  :: deathCount(3) ! Death count 
    integer :: timeStep               ! Time step
    integer :: popSize                ! Population size

    integer, pointer :: deathByAge
    integer, pointer :: deathByMutation
    integer, pointer :: deathByVerhulst

    ! Initialization
    popSize = startPopSize
    deathCount(:) = 0
    call resetAgeDstrb(MODEL_L)
    call initRunWriter(runWriter, recordFlag)
    population = Population_t(startPopSize, initMttnCount)

    ! Initialize pointers.
    deathByAge => deathCount(1)
    deathByMutation => deathCount(2)
    deathByVerhulst => deathCount(3)

    ! Enable/disable demographics recording.
    if (recordFlag == REC_AGE_DSTRB) then
      DEMOG_LAST_STEPS = DEF_DEMOG_LAST_STEP
    else
      DEMOG_LAST_STEPS = -1
    end if

    ! Record data of the initial state of the population.
    ! The data that would be obtained at this point in the program
    ! represent the data at t = 0.
    call recordData(recordFlag)

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
          recordFlag, deathByAge, deathByMutation, deathByVerhulst)
      popSize = population%getPopSize()

      ! Record data.
      call recordData(recordFlag)

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
    subroutine recordData(charFlag)
      character(len=*), intent(in) :: charFlag
        !! Record flag.

      ! Write data into a file as specified by `charFlag`.
      select case (charFlag)
        case (REC_POP)
          call runWriter % write(int(popSize, kind=writeIK))

        case (REC_AGE_DSTRB)
          call runWriter % write(int(ageDistribution, kind=writeIK))

        case (REC_DEATH)
          call runWriter % write(int(deathCount, kind=writeIK))

        case (REC_DIV_IDX)
          call runWriter % write(real(getDiversityIdx(), kind=writeRK))
        
        case (REC_GENE_DSTRB)
          call runWriter % write(int(getBadGeneDstrb(MODEL_L), kind=writeIK))
      end select
    end subroutine recordData
  end subroutine runOneInstance


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalPopulation
  !>  Evaluate the population for one time step.
  ! -------------------------------------------------------------------------- !
  subroutine evalPopulation( &
      population,       &
      countdown,        &
      recordFlag,       &
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
    character,          intent(in)    :: recordFlag       !! Record flag

    type(Person_t), pointer :: currPerson

    evalPop: do while(.not. population%atEndOfPopulation())
      ! Evaluate the current person. If this person is alive, its age is
      ! incremented and birth event is checked.
      call population%evalCurrPerson()

      currPerson => defaultPersonPtr(population%getCurrPerson())
      if (.not.associated(currPerson)) then
        call raiseError("Internal error. Null current `Person_t` pointer.")
      end if

      if (currPerson%lifeStat == ALIVE) then
        ! Update the genome distribution.
        if (any(recordFlag == [REC_DIV_IDX, REC_GENE_DSTRB])) then
          call updateGenomeDstrb(currPerson%genome)
        end if
      else
        if (recordFlag == REC_DEATH) then
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
  ! SUBROUTINE: runMultipleInstance
  !>  Run the Penna model simulation many times.
  ! -------------------------------------------------------------------------- !
  subroutine runMultipleInstance( &
      maxTimeStep,    &
      startPopSize,   &
      initMttnCount,  &
      recordFlag,     &
      sampleSize,     &
      recordTime,     &
      printProgress   &
    )

    integer,   intent(in) :: maxTimeStep
      !! Maximum (total) time step.
    integer,   intent(in) :: startPopSize
      !! Starting population size.
    integer,   intent(in) :: initMttnCount
      !! Initial mutation count of each individuals.
    character, intent(in) :: recordFlag
      !! Record flag. Valid values are found in the `WriterOptions` module.
    integer,   intent(in) :: sampleSize
      !! Sample size. Number of times the simulation is run.
    logical,   intent(in) :: recordTime
      !! Record mean elapsed time and the corresponding standard deviation.
    logical,   intent(in) :: printProgress
      !! Print the progress with a progress bar.

    real(kind=timeRK)    :: meanTime
    real(kind=timeRK)    :: stdDevTime
    integer(kind=timeIK) :: startTimeInt
    integer(kind=timeIK) :: endTimeInt
    real(kind=timeRK)    :: startTimeReal
    real(kind=timeRK)    :: endTimeReal
    real(kind=timeRK)    :: clockRate
    real(kind=timeRK)    :: sum
    real(kind=timeRK)    :: sumSqrd

    type(ProgressBar) :: progBar    ! A `ProgressBar` object.
    type(Writer)      :: timeWriter ! A `Writer` object for writing timings.
    integer :: i

    ! Print separator for pretty printing.
    character, parameter :: PRINT_SEPARATOR(*) = [("=", i = 1, 29)]
    ! Unit for writing timing statistics.
    integer, parameter :: TIME_WRITER_UNIT = 101

    ! Initialize the progress bar.
    progBar = ProgressBar(20, sampleSize)

    ! Call and time the `run` subroutine.
    sum = 0._timeRK
    sumSqrd = 0._timeRK
    do i = 1, sampleSize
      ! Start timer.
      call system_clock(count=startTimeInt, count_rate=clockRate)  
      startTimeReal = real(startTimeInt, kind=timeRK)/clockRate

      ! Run the actual simulation.
      call runOneInstance(maxTimeStep, startPopSize, initMttnCount, &
          recordFlag)

      ! End timer.
      call system_clock(count=endTimeInt, count_rate=clockRate)
      endTimeReal = real(endTimeInt, kind=timeRK)/clockRate

      ! Calculate necessary values for average and std deviation.
      sum = sum + (endTimeReal - startTimeReal)*1e3
      sumSqrd = sumSqrd + ((endTimeReal - startTimeReal)*1e3)**2

      ! Print the progress bar.
      if (printProgress .and. sampleSize > 1) then
        call progBar % incrCounter(show=.true.)
      end if
    end do

    ! Remove the progress bar.
    write(*, "(*(a))", advance="no") (char(8), i = 1, 30)

    ! Get average elapsed time and its std deviation.
    meanTime = sum/real(sampleSize, kind=timeRK)
    stdDevTime = sqrt(sampleSize*sumSqrd - sum**2) / &
      real(sampleSize, kind=timeRK)

    ! Print timing statistics.
    if (printProgress) then
      ! Print elapsed time.
      if (sampleSize > 1) then
        print "(a, f12.3, a)", "Average time: ", meanTime, " ms"
      else
        print "(a, f12.3, a)", "Elapsed time: ", meanTime, " ms"
      end if

      ! Print the standard deviation.
      if (sampleSize > 1) &
        print "(a, f11.3, a)", "Std deviation: ", stdDevTime, " ms"
    end if

    ! Record mean time and std deviation.
    if (recordTime) then
      timeWriter = Writer(PROG_TIME_FILE_NAME, TIME_WRITER_UNIT)
      call timeWriter % openFile()

      ! Write the header of the file.
      call timeWriter % write("DATA: Timing statistics")
      call timeWriter % write([(FILE_DIVIDER, i = 1, 5)])
      call timeWriter % write( &
        ["Max Time Step ", &
         "Init pop size ", &
         "Sample size   ", &
         "Ave. time (ms)", &
         "Std. dev. (ms)"])
      call timeWriter % write([(FILE_DIVIDER, i = 1, 5)])
        
      ! Write the actual timing statistics.
      call timeWriter % write([             &
          real(maxTimeStep, kind=writeRK),  &
          real(startPopSize, kind=writeRK), &
          real(SampleSize, kind=writeRK),   &
          real(meanTime, kind=writeRK),     &
          real(stdDevTime, kind=writeRK)]   &
        )
    end if

    if (printProgress) print "(*(a))", PRINT_SEPARATOR
  end subroutine runMultipleInstance


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: run
  !>  Run the Penna model simulation. This is a wrapper subroutine to the
  !!  subroutine `runMultipleInstance`.
  ! -------------------------------------------------------------------------- !
  subroutine run()
    logical :: printProgress
    printProgress = PROG_PRINT_STATE /= SILENT_PRINT

    call runMultipleInstance( &
        MODEL_TIME_STEPS,     &
        MODEL_START_POP_SIZE, &
        MODEL_MTTN_COUNT,     &
        PROG_REC_FLAG,        &
        PROG_SAMPLE_SIZE,     &
        PROG_RECORD_TIME,     &
        printProgress         &
        )
  end subroutine run


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initRunWriter
  !>  Initialize a `Writer` object based on the integer `recordFlag`
  !!  passed. The flags are defined in the `CmdOptions` module.
  ! -------------------------------------------------------------------------- !
  subroutine initRunWriter(runWriter, recordFlag)
    type(Writer), intent(inout) :: runWriter
      !! The `Writer` object to be initialized.
    character,    intent(in)    :: recordFlag
      !! Record flag. Values can be found in `WriterOptions`.

    integer, parameter :: RUN_WRITER_UNIT = 100

    character(len=15), allocatable :: headerArr(:)
    integer :: i, startingAge

    if (recordFlag == REC_NULL) return

    ! Initialize writer.
    runWriter = Writer(PROG_OUT_FILE_NAME, RUN_WRITER_UNIT)
    ! Open file for writing.
    call runWriter % openFile()

    ! Append the header to the file to be written on.
    select case (recordFlag)
      case (REC_POP)
        ! Data description.
        call runWriter % write("DATA: Population size per time step")

        ! Header of the list.
        call runWriter % write([FILE_DIVIDER])
        call runWriter % write(["Population size"])
        call runWriter % write([FILE_DIVIDER])


      case (REC_GENE_DSTRB, REC_AGE_DSTRB)
        ! Data description.
        if (recordFlag == REC_GENE_DSTRB) then
          call runWriter % write("DATA: Bad gene distribution per time step")
          startingAge = 1  ! Get the starting age of the distribution as well.
        else
          call runWriter % write("DATA: Age distribution per time step")
          startingAge = 0  ! Get the starting age of the distribution as well.
        end if

        ! For some reason, implicit do loop truncate numbers.
        allocate(headerArr(startingAge:MODEL_L))
        do i = startingAge, MODEL_L
          headerArr(i) = "AGE " // trim(castIntToChar(i))
        end do

        ! Header of the table.
        call runWriter % write([(FILE_DIVIDER, i = startingAge, MODEL_L)])
        call runWriter % write(headerArr)
        call runWriter % write([(FILE_DIVIDER, i = startingAge, MODEL_L)])


      case (REC_DEATH)
        ! Data description.
        call runWriter % write("DATA: Number of deaths " // &
            "(due to old age, mutation, Verhulst factor) per time step")
        
        ! Header of the table.
        call runWriter % write([(FILE_DIVIDER, i = 1, 3)])
        call runWriter % write( &
            ["Old age        ", &
             "Mutation       ", &
             "Verhulst factor"])
        call runWriter % write([(FILE_DIVIDER, i = 1, 3)])
        

      case (REC_DIV_IDX)
        ! Data description. 
        call runWriter % write("DATA: Shannon diversity index per time step.")

        ! Header of the list.
        call runWriter % write([FILE_DIVIDER])
        call runWriter % write(["Diversity idx"])
        call runWriter % write([FILE_DIVIDER])


      case default
        call raiseError("'" // recordFlag //"' is an invalid record flag")
    end select

    if (allocated(headerArr)) deallocate(headerArr)
  end subroutine initRunWriter
end module Penna

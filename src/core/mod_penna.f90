module Penna
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Penna
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing one of the core procedures for the simulation of the
  !!  Penna model (along with the `Pop` module)
  ! -------------------------------------------------------------------------- !
  use Parameters, only:   &
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
    SILENT_PRINT,         &
    setParams,            &
    printProgDetails,     &
    freeParamAlloctbls
  use RNG, only: assignRNGParams
  use ProgBarType, only: ProgressBar
  use CastProcedures, only: castIntToChar  
  use ErrorMSG, only: raiseError, raiseWarning
  
  ! WARNING: Implicit import. This module needs all its public components.
  use Pop
  use Demographics
  use WriterOptions
  implicit none
  private

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

    ! Initialize the writer objects.
    call initWriterObjs()
  end subroutine initProgram


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeAlloctbls
  !>  A wrapper subroutine to deallocate any allocatable variables in other
  !!  module.
  ! -------------------------------------------------------------------------- !
  subroutine freeAlloctbls()
    call freeWriterModAlloctbls()
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
    call resetAgeDstrb()
    call initRunWriter(runWriter, recordFlag)
    call initializePersonList(startPopSize, initMttnCount)

    ! Initialize pointers.
    deathByAge => deathCount(1)
    deathByMutation => deathCount(2)
    deathByVerhulst => deathCount(3)

    ! Enable/disable demographics recording.
    if (recordFlag == ageDstrbFlag) then
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
      call evalPopulation(popSize, maxTimestep - timeStep, &
          recordFlag, deathByAge, deathByMutation, deathByVerhulst)

      ! Record data.
      call recordData(recordFlag)

      ! Reset variables for the next time step.
      deathCount(:) = 0
      call resetPersonReadPtrs()
      if (recordFlag == ageDstrbFlag) call resetAgeDstrb()
      if (recordFlag == divIdxFlag .or. recordFlag == badGeneFlag) &
          call freeGenomeDstrbList()
    end do mainLoop

    ! Wrap up.
    call freePersonPtrs(popSize)
    call runWriter % close()
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
        case (popFlag)
          call runWriter % write(popFlag, int(popSize, kind=writeIK))

        case (ageDstrbFlag)
          call runWriter % write(ageDstrbFlag, &
              int(ageDistribution, kind=writeIK))

        case (deathFlag)
          call runWriter % write(deathFlag, int(deathCount, kind=writeIK))

        case (divIdxFlag)
          call runWriter % write(divIdxFlag, &
              real(getDiversityIdx(), kind=writeRK))
        
        case (badGeneFlag)
          call runWriter % write(badGeneFlag, &
              int(getBadGeneDstrb(), kind=writeIK))
      end select
    end subroutine recordData
  end subroutine runOneInstance


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalPopulation
  !>  Evaluate the population.
  ! -------------------------------------------------------------------------- !
  subroutine evalPopulation(&
      popSize,          &
      countdown,        &
      recordFlag,       &
      deathByAge,       &
      deathByMutation,  &
      deathByVerhulst   &
      )

    integer,          intent(inout) :: popSize          !! Population size.
    integer, pointer, intent(inout) :: deathByAge       !! Death by age count.
    integer, pointer, intent(inout) :: deathByMutation  !! Death by mutation.
    integer, pointer, intent(inout) :: deathByVerhulst  !! Random death.
    integer,          intent(in)    :: countdown        !! Count from max time.
    character,        intent(in)    :: recordFlag       !! Record flag.

    integer :: popSizeChange
    integer :: listStatus

    ! Initialize variables
    popSizeChange = 0
    evalPop: do
      ! Catch extinction case.
      if (popSize == 0) exit

      ! Evaluate the current individual. 
      call checkCurrIndivDeath(popSize)
      if (isCurrIndivDead()) then
        ! Get the cause of death of the current dead individual.
        call determineDeathType(deathByAge, deathByMutation, deathByVerhulst)

        popSizeChange = popSizeChange - 1
      else
        ! Update age of the alive individuals.
        call updateCurrIndivAge()

        ! Update genome distribution.
        if (recordFlag == divIdxFlag .or. recordFlag == badGeneFlag) &
            call updateGenomeDstrb(getCurrIndivGenome())

        ! Check for birth events.
        if (isCurrIndivMature()) then
          call reproduceCurrIndiv(recordFlag == divIdxFlag)
          popSizeChange = popSizeChange + MODEL_B
        end if
      end if

      ! Record demographics.
      if (countdown <= DEMOG_LAST_STEPS) &
          call updateAgeDstrb(getCurrIndivAge(), ageDistribution)

      ! Proceed to the next element of the linked list.
      call nextElem(listStatus)
      ! Exit condition.
      if (listStatus /= 0) exit
    end do evalPop
  
    ! Update population size
    popSize = popSize + popSizeChange
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

    real(kind=writeRK)    :: meanTime
    real(kind=writeRK)    :: stdDevTime
    integer(kind=writeIK) :: startTimeInt
    integer(kind=writeIK) :: endTimeInt
    real(kind=writeRK)    :: startTimeReal
    real(kind=writeRK)    :: endTimeReal
    real(kind=writeRK)    :: clockRate
    real(kind=writeRK)    :: sum
    real(kind=writeRK)    :: sumSqrd

    type(ProgressBar) :: progBar    ! A `ProgressBar` object.
    type(Writer)      :: timeWriter ! A `Writer` object for writing timings.
    integer :: i

    ! Print separator for pretty printing.
    character, parameter :: PRINT_SEPARATOR(*) = [("=", i = 1, 29)]

    ! Initialize the progress bar.
    call progBar % init(20, sampleSize)

    ! Call and time the `run` subroutine.
    sum = 0.
    sumSqrd = 0.
    do i = 1, sampleSize
      ! Start timer.
      call system_clock(count=startTimeInt, count_rate=clockRate)  
      startTimeReal = real(startTimeInt, kind=writeRK)/clockRate

      ! Run the actual simulation.
      call runOneInstance(maxTimeStep, startPopSize, initMttnCount, &
          recordFlag)

      ! End timer.
      call system_clock(count=endTimeInt, count_rate=clockRate)
      endTimeReal = real(endTimeInt, kind=writeRK)/clockRate

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
    meanTime = sum/real(sampleSize, kind=writeRK)
    stdDevTime = sqrt(sampleSize*sumSqrd - sum**2) / &
      real(sampleSize, kind=writeRK)

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
      call constructAvailableWriter(timeWriter, [timeFlag], .true.)
      call timeWriter % writeHeader(timeFlag, &
          ["max time step       ", &
           "initial pop size    ", &
           "average time (ms)   ", &
           "std deviation (ms)  "])
      call timeWriter % write(timeFlag, &
          [real(maxTimeStep, kind=writeRK), &
           real(startPopSize, kind=writeRK), &
           meanTime, &
           stdDevTime])
      call timeWriter % close()
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

    if (recordFlag == nullFlag) return

    ! Construct the `Writer` type.
    call constructAvailableWriter(runWriter, &
        [popFlag, ageDstrbFlag, deathFlag, divIdxFlag, badGeneFlag])

    call runWriter % initialize(recordFlag)
    select case (recordFlag)
      case (popFlag)
        call runWriter % writeHeader(popFlag, ["population size"])

      case (ageDstrbFlag)
        call runWriter % writeHeader(ageDstrbFlag, ["age =>"])

      case (deathFlag)
        call runWriter % writeHeader(deathFlag, &
            ["death by old age        ", &
            "death by mutation       ", &
            "death by Verhulst factor"])

      case (divIdxFlag)
        call runWriter % writeHeader(divIdxFlag, &
            ["Diversity index per time step"])
      
      case (badGeneFlag)
        call runWriter % writeHeader(badGeneFlag, ["age =>"])

      case default
        call raiseError("'" // trim(recordFlag) //"' is an invalid record flag")
    end select
  end subroutine initRunWriter
end module Penna

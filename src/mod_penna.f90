module Penna
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Penna
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing one of the core procedures for the simulation of the
  !!  Penna model (along with the `Pop` module)
  ! -------------------------------------------------------------------------- !
  use Pop
  use ModelParam
  use WriterOptions
  implicit none
  private

  public :: run
  public :: deallocAllocatables
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocAllocatables
  !>  A wrapper subroutine to deallocate any allocatable variables in other
  !!  module.
  ! -------------------------------------------------------------------------- !
  subroutine deallocAllocatables()
    use ModelParam, only: deallocVerhulstWeights
    use WriterOptions, only: deallocWriterTypeAlloctbl
    use Demographics, only: deallocAgeDstrb
  
    call deallocVerhulstWeights()
    call deallocWriterTypeAlloctbl()
    call deallocAgeDstrb()
  end subroutine deallocAllocatables


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: runOneInstance
  !>  Run the Penna model simulation once.
  ! -------------------------------------------------------------------------- !
  subroutine runOneInstance(maxTimestep, startPopSize, recordFlag)
    use Demographics

    integer, intent(in) :: maxTimestep
      !! Maximum (total) time step.
    integer, intent(in) :: startPopSize
      !! Starting population size.
    integer, intent(in) :: recordFlag
      !! Record flag. Valid values are found in the `WriterOptions` module.

    type(PersonList) :: population    ! Population list
    type(Writer)     :: runWriter     ! `Writer` object for recording data 
    integer, target  :: deathCount(3) ! Death count 
    integer :: timeStep               ! Time step
    integer :: popSize                ! Population size

    integer, pointer :: deathByAge
    integer, pointer :: deathByMutation
    integer, pointer :: deathByVerhulst

    ! Initialize variables
    population = constructPersonList(startPopSize)
    popSize = startPopSize
    deathCount(:) = 0
    call resetAgeDstrb()
    call initializeRunWriter(runWriter, recordFlag)

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

    ! Run the model.
    mainLoop: do timeStep = 1, maxTimestep
      ! Catch case when pop size exceeds the carrying capacity.
      ! For this given set of model parameters, pop size might explode.
      ! As such, we halt the run once this case is reached.
      if (popSize > MODEL_K) then
        print "(/a)", "The population has exceeded the carrying capacity!" // &
            " Stopping the current run."
        exit
      end if

      ! Evaluate population
      call evalPopulation(population, popSize, maxTimestep - timeStep, &
        recordFlag, deathByAge, deathByMutation, deathByVerhulst)

      ! Record result.
      select case (recordFlag)
        case (popFlag)
          call runWriter % write(popFlag, int(popSize, kind=writeIK))

        case (ageDstrbFlag)
          call runWriter % write((ageDstrbFlag), &
              int(ageDistribution, kind=writeIK))

        case (deathFlag)
          call runWriter % write(deathFlag, int(deathCount, kind=writeIK))

        case (divIdxFlag)
          call runWriter % write(divIdxFlag, &
              real(getDiversityIdx(), kind=writeRK))
      end select

      ! Reset variables for the next time step.
      deathCount(:) = 0
      call population % resetReadPtrs()
      if (recordFlag == ageDstrbFlag) call resetAgeDstrb()
      if (recordFlag == divIdxFlag) call freeGenomeDstrbList()
    end do mainLoop

    ! Wrap up.
    call population % freePtr(popSize)
    call runWriter % close()
  end subroutine runOneInstance


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalPopulation
  !>  Evaluate the population.
  ! -------------------------------------------------------------------------- !
  subroutine evalPopulation(population, popSize, countdown, recFlag, &
        deathByAge, deathByMutation, deathByVerhulst)
    use Demographics

    type(PersonList), intent(inout) :: population       !! Population object.
    integer,          intent(inout) :: popSize          !! Population size.
    integer, pointer, intent(inout) :: deathByAge       !! Death by age count.
    integer, pointer, intent(inout) :: deathByMutation  !! Death by mutation.
    integer, pointer, intent(inout) :: deathByVerhulst  !! Random death.
    integer,          intent(in)    :: countdown        !! Count from max time.
    integer,          intent(in)    :: recFlag          !! Record flag.

    integer :: popSizeChange
    integer :: listStatus

    ! Initialize variables
    popSizeChange = 0
    evalPop: do
      ! Catch extinction case.
      if (popSize == 0) exit

      ! Evaluate the current individual. 
      call population % checkCurrIndivDeath(popSize)

      if (population % isCurrIndivDead()) then
        ! Get the cause of death of the current dead individual.
        call population % determineDeathType(deathByAge, deathByMutation, &
            deathByVerhulst)

        popSizeChange = popSizeChange - 1
      else
        ! Update age of the alive individuals.
        call population % updateCurrIndivAge()

        ! Update genome distribution.
        if (recFlag == divIdxFlag) &
            call updateGenomeDstrb(population % getCurrIndivGenome())

        ! Check for birth events.
        if (population % isCurrIndivMature()) then
          call population % reproduceCurrIndiv(recFlag == divIdxFlag)
          popSizeChange = popSizeChange + MODEL_B
        end if
      end if

      ! Record demographics.
      if (countdown <= DEMOG_LAST_STEPS) &
          call updateAgeDstrb(population % getCurrIndivAge(), ageDistribution)

      ! Proceed to the next element of the linked list.
      call population % nextElem(listStatus)
      ! Exit condition.
      if (listStatus /= 0) exit
    end do evalPop
  
    ! Update population size
    popSize = popSize + popSizeChange
  end subroutine evalPopulation


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: run
  !>  Run the Penna model simulation.
  ! -------------------------------------------------------------------------- !
  subroutine run()
    use ProgBarType

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
    logical :: printProgress

    ! Print separator for pretty printing.
    character, parameter :: PRINT_SEPARATOR(*) = [("=", i = 1, 29)]

    printProgress = PROG_PRINT_STATE /= SILENT_PRINT

    ! Initialize the writer objects.
    call initializeWriterObjects()

    ! Initialize the progress bar.
    call initProgressBar(progBar, 20, MODEL_SAMPLE_SIZE)

    ! Call and time the `run` subroutine.
    sum = 0.
    sumSqrd = 0.
    do i = 1, MODEL_SAMPLE_SIZE
      ! Start timer.
      call system_clock(count=startTimeInt, count_rate=clockRate)  
      startTimeReal = real(startTimeInt, kind=writeRK)/clockRate

      ! Run the actual simulation.
      call runOneInstance(MODEL_TIME_STEPS, MODEL_N0, MODEL_REC_FLAG)

      ! End timer.
      call system_clock(count=endTimeInt, count_rate=clockRate)
      endTimeReal = real(endTimeInt, kind=writeRK)/clockRate

      ! Calculate necessary values for average and std deviation.
      sum = sum + (endTimeReal - startTimeReal)*1e3
      sumSqrd = sumSqrd + ((endTimeReal - startTimeReal)*1e3)**2

      ! Print the progress bar.
      if (printProgress .and. MODEL_SAMPLE_SIZE > 1) then
        call progBar % incrementCounter(show=.true.)
      end if
    end do

    ! Remove the progress bar.
    write(*, "(*(a))", advance="no") (char(8), i = 1, 30)

    ! Get average elapsed time and its std deviation.
    meanTime = sum/real(MODEL_SAMPLE_SIZE, kind=writeRK)
    stdDevTime = sqrt(MODEL_SAMPLE_SIZE*sumSqrd - sum**2) / &
      real(MODEL_SAMPLE_SIZE, kind=writeRK)

    ! Print timing statistics.
    if (printProgress) then
      ! Print elapsed time.
      if (MODEL_SAMPLE_SIZE > 1) then
        print "(a, f12.3, a)", "Average time: ", meanTime, " ms"
      else
        print "(a, f12.3, a)", "Elapsed time: ", meanTime, " ms"
      end if

      ! Print the standard deviation.
      if (MODEL_SAMPLE_SIZE > 1) &
        print "(a, f11.3, a)", "Std deviation: ", stdDevTime, " ms"
    end if

    ! Record mean time and std deviation.
    if (PROG_RECORD_TIME) then
      call constructAvailableWriter(timeWriter, [timeFlag], .true.)
      call timeWriter % writeHeader(timeFlag, &
          ["max time step       ", &
           "initial pop size    ", &
           "average time (ms)   ", &
           "std deviation (ms)  "])
      call timeWriter % write(timeFlag, &
          [real(MODEL_TIME_STEPS, kind=writeRK), &
           real(MODEL_N0, kind=writeRK), &
           meanTime, &
           stdDevTime])
      call timeWriter % close()
    end if

    if (printProgress) print "(*(a))", PRINT_SEPARATOR
  end subroutine run


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeRunWriter
  !>  Initialize a `Writer` object based on the integer `recordFlag`
  !!  passed.  There are three flags: `popFlag`, `ageDstrbFlag`
  !!  and `death_recflag`.
  ! -------------------------------------------------------------------------- !
  subroutine initializeRunWriter(runWriter, recordFlag)
    type(Writer), intent(inout) :: runWriter
      !! The `Writer` object to be initialized.
    integer,      intent(in)    :: recordFlag
      !! Record flag. Values can be found in `WriterOptions`.

    if (recordFlag == nullFlag) return

    ! Construct the `Writer` type.
    call constructAvailableWriter(runWriter, &
        [popFlag, ageDstrbFlag, deathFlag, divIdxFlag])

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

      case default
        print "(a, i0, a)", "***ERROR. '", recordFlag, &
            "' is an invalid record flag"
        stop
    end select
  end subroutine initializeRunWriter
end module Penna

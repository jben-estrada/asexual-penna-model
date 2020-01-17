module Penna
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Penna
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing one of the core procedures for the simulation of the
  !!  Penna model (along with the `Pop` module)
  ! -------------------------------------------------------------------------- !
  use Pop
  use WriterType
  use SaveFormat
  implicit none
  private

  public :: run
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: runOneInstance
  !>  Run the Penna model simulation once.
  ! -------------------------------------------------------------------------- !
  subroutine runOneInstance(maxTimestep, startPopSize, recordFlag)
    use Demographics
    use ModelParam, only: MODEL_K
    implicit none

    integer, intent(in) :: maxTimestep
    integer, intent(in) :: startPopSize
    integer, intent(in) :: recordFlag

    type(PersonList) :: population  ! Population list
    type(Writer)     :: runWriter   ! `Writer` object for recording data 
    integer :: timeStep             ! Time step
    integer :: popSize              ! Population size
    integer :: deathCount(3)        ! Death count 
    ! NOTE: The elements of `deathCount` correspond to the following deaths:
    !       1.) Death by old age, 2.) Death by mutation, 3.) Verhulst death

    ! Initialize variables
    population = constructPersonList(startPopSize)
    popSize = startPopSize
    deathCount(:) = 0
    call resetAgeDstrb()
    call initializeRunWriter(runWriter, recordFlag)

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
      call evalPopulation(population, deathCount, popSize, &
          maxTimestep - timeStep, recordFlag)

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
    call population  %  freePtr(popSize)
    call runWriter  %  close()
    call deallocAgeDstrb()
  end subroutine runOneInstance


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalPopulation
  !>  Evaluate the population.
  ! -------------------------------------------------------------------------- !
  subroutine evalPopulation(population, deathCount, popSize, countdown, recFlag)
    use Demographics
    use ModelParam, only: MODEL_B, DEAD_MUTATION, DEAD_OLD_AGE, DEAD_VERHULST
    implicit none

    type(PersonList), intent(inout) :: population
    integer,          intent(inout) :: deathCount(3)
    integer,          intent(inout) :: popSize
    integer,          intent(in)    :: countdown
    integer,          intent(in)    :: recFlag

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
        ! Count dead ones.
        select case (population % getCurrIndivDeathIdx())
          case (DEAD_OLD_AGE)
            deathCount(1) = deathCount(1) + 1

          case (DEAD_MUTATION)
            deathCount(2) = deathCount(2) + 1

          case (DEAD_VERHULST)
            deathCount(3) = deathCount(3) + 1

          case default
            stop "Dead `Person` object has an invalid death index."
        end select

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
  subroutine run(maxTimeStep, startingPopSize, sampleSize, recordFlag, &
        toRecordTime, printRunProgress)
    use ProgBarType
    use ModelParam, only: PRINT_SEPARATOR
    implicit none

    integer, intent(in) :: maxTimeStep
    integer, intent(in) :: sampleSize   
    integer, intent(in) :: startingPopSize
    integer, intent(in) :: recordFlag
    logical, intent(in) :: toRecordTime
    logical, intent(in) :: printRunProgress

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

    ! Initialize the progress bar.
    call initProgressBar(progBar, 20, sampleSize)

    ! Call and time the `run` subroutine.
    sum = 0.
    sumSqrd = 0.
    do i = 1, sampleSize
      ! Start timer.
      call system_clock(count=startTimeInt, count_rate=clockRate)  
      startTimeReal = real(startTimeInt, kind=writeRK)/clockRate

      ! Run the actual simulation.
      call runOneInstance(maxTimeStep, startingPopSize, recordFlag)

      ! End timer.
      call system_clock(count=endTimeInt, count_rate=clockRate)
      endTimeReal = real(endTimeInt, kind=writeRK)/clockRate

      ! Calculate necessary values for average and std deviation.
      sum = sum + (endTimeReal - startTimeReal)*1e3
      sumSqrd = sumSqrd + ((endTimeReal - startTimeReal)*1e3)**2

      ! Print the progress bar.
      if (printRunProgress) then
        call progBar % incrementCounter(show=.true.)
      end if
    end do

    ! Get average elapsed time and its std deviation.
    meanTime = sum/real(sampleSize, kind=writeRK)
    stdDevTime = sqrt(sampleSize*sumSqrd - sum**2) / &
      real(sampleSize, kind=writeRK)

    ! Print timing statistics.
    if (printRunProgress) then
      ! Print elapsed time.
      if (sampleSize > 1) then
        print "(/a, f12.3, a)", "Average time: ", meanTime, " ms"
      else
        print "(/a, f12.3, a)", "Elapsed time: ", meanTime, " ms"
      end if

      ! Print the standard deviation.
      if (sampleSize > 1) &
        print "(a, f11.3, a)", "Std deviation: ", stdDevTime, " ms"
    end if

    ! Record mean time and std deviation.
    if (toRecordTime) then
      call constructWriter(timeWriter, [timeFlag])
      call timeWriter % initialize()
      call timeWriter % writeHeader(timeFlag, &
          ["max time step       ", &
           "initial pop size    ", &
           "average elapsed time", &
           "standard deviation  "])
      call timeWriter % write(timeFlag, &
          [real(maxTimeStep, kind=writeRK), &
           real(startingPopSize, kind=writeRK), &
           meanTime, &
           stdDevTime])
      call timeWriter % close()
    end if

    if (printRunProgress) print "(*(a))", PRINT_SEPARATOR
  end subroutine run


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeRunWriter
  !>  Initialize a `Writer` object based on the integer `recordFlag`
  !!  passed.  There are three flags: `popFlag`, `ageDstrbFlag`
  !!  and `death_recflag`.
  ! -------------------------------------------------------------------------- !
  subroutine initializeRunWriter(runWriter, recordFlag)
    implicit none

    type(Writer), intent(inout) :: runWriter
    integer,      intent(in)    :: recordFlag

    call constructWriter(runWriter, formatFlags)

    if (recordFlag == nullFlag) return

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

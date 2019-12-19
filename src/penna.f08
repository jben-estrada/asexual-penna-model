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
  use ModelParam
  implicit none
  private

  ! Record flags. TODO: Allow multiple flags.
  integer, parameter, public :: nullRecFlag = 0  ! Null
  integer, parameter, public :: popRecFlag = 1   ! Population
  integer, parameter, public :: demogRecFlag = 2 ! Age and genome demographics
  integer, parameter, public :: deathRecFlag = 3 ! Death count

  public :: multipleRun
  public :: readModelParam
  public :: wrapUp
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readModelParam
  !>  Wrapper procedure for `readScalarParam`
  ! -------------------------------------------------------------------------- !
  subroutine readModelParam()
    implicit none

    call readScalarParam
    call readVerhulstWeights
  end subroutine readModelParam


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: run
  !>  Run the Penna model.
  !   TODO: Seems to be too long. Could be separated into different procedures. 
  ! -------------------------------------------------------------------------- !
  subroutine run(maxTimestep, startPopSize, recordFlag)
    use Demographics
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
    call resetDstrbs()
    call initializeRunWriter(runWriter, recordFlag)

    ! Enable/disable demographics recording.
    if (recordFlag == demogRecFlag) then
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
          maxTimestep - timeStep)

      ! Record result.
      select case (recordFlag)
        case (popRecFlag)
          call runWriter%write(popFlag, int(popSize, kind=writeIK))

        case (demogRecFlag)
          call runWriter%write((ageDstrbFlag), &
              int(demog_ageDstrb, kind=writeIK))
          call runWriter%write(genomeDstrbFlag, &
              int(demog_genomeDstrb, kind=writeIK))

        case (deathRecFlag)
          call runWriter%write(deathFlag, int(deathCount, kind=writeIK))
      end select

      ! Reset variables for the next time step.
      deathCount(:) = 0
      call population%resetReadPtrs()
      if (recordFlag == demogRecFlag) call resetDstrbs()
    end do mainLoop

    ! Wrap up.
    call population%freePtr(popSize)
    call runWriter%close()
    call deallocDstrb()
  end subroutine run


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalPopulation
  !>  Evaluate the population, i.e. 
  ! -------------------------------------------------------------------------- !
  subroutine evalPopulation(population, deathCount, popSize, countdown)
    use Flag
    use Demographics
    implicit none

    type(PersonList), intent(inout) :: population
    integer,          intent(inout) :: deathCount(3)
    integer,          intent(inout) :: popSize
    integer,          intent(in)    :: countdown

    integer :: popSizeChange
    integer :: listStatus

    ! Initialize variables
    popSizeChange = 0
    evalPop: do
      ! Catch extinction case.
      if (popSize == 0) exit

      ! Evaluate the current individual. 
      call population%checkCurrIndivDeath(popSize)

      if (population%isCurrIndivDead()) then
        ! Count dead ones.
        select case (population%getCurrIndivDeathIdx())
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
        call population%updateCurrIndivAge()

        ! Check for birth events.
        if (population%isCurrIndivMature()) then
          call population%reproduceCurrIndiv()
          popSizeChange = popSizeChange + MODEL_B
        end if
      end if

      ! Record demographics.
      if (countdown <= DEMOG_LAST_STEPS) then
        call updateAgeDstrb(population%getCurrIndivAge(), demog_ageDstrb)
        call updateGenomeDstrb(population%getCurrIndivGenome(), &
            demog_genomeDstrb)
      end if

      ! Proceed to the next element of the linked list.
      call population%nextElem(listStatus)
      ! Exit condition.
      if (listStatus /= 0) exit
    end do evalPop
  
    ! Update population size
    popSize = popSize + popSizeChange
  end subroutine evalPopulation


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: multipleRun
  !>  Call the `run` subroutine and time it for `sampleSize` times.
  ! -------------------------------------------------------------------------- !
  subroutine multipleRun(maxTimeStep, startingPopSize, sampleSize, recordFlag, &
        toRecordTime)
    use TickerType
    implicit none

    integer, intent(in) :: maxTimeStep
    integer, intent(in) :: sampleSize   
    integer, intent(in) :: startingPopSize
    integer, intent(in) :: recordFlag
    logical, intent(in) :: toRecordTime

    real(kind=writeRK)    :: meanTime
    real(kind=writeRK)    :: stdDevTime
    integer(kind=writeIK) :: startTimeInt
    integer(kind=writeIK) :: endTimeInt
    real(kind=writeRK)    :: startTimeReal
    real(kind=writeRK)    :: endTimeReal
    real(kind=writeRK)    :: clockRate
    real(kind=writeRK)    :: sum
    real(kind=writeRK)    :: sumSqrd

    type(Writer) :: timeWriter    ! `Writer` object to write timings stats
    type(Ticker) :: runTicker     ! `Ticker` object for the fancy progress bar
    integer :: i

    ! Initialize `runTicker`.
    runTicker = constructTicker(20, sampleSize)

    ! Call and time the `run` subroutine.
    sum = 0.
    sumSqrd = 0.
    do i = 1, sampleSize
      ! Start timer.
      call system_clock(count=startTimeInt, count_rate=clockRate)  
      startTimeReal = real(startTimeInt, kind=writeRK)/clockRate

      ! Run the actual simulation.
      call run(maxTimeStep, startingPopSize, recordFlag)

      ! End timer.
      call system_clock(count=endTimeInt, count_rate=clockRate)
      endTimeReal = real(endTimeInt, kind=writeRK)/clockRate

      ! Calculate necessary values for average and std deviation.
      sum = sum + (endTimeReal - startTimeReal)*1e3
      sumSqrd = sumSqrd + ((endTimeReal - startTimeReal)*1e3)**2

      ! Print progress bar
      call runTicker%incrementTick(show=.true.)
    end do

    ! Get average elapsed time and its std deviation.
    meanTime = sum/real(sampleSize, kind=writeRK)
    stdDevTime = sqrt(sampleSize*sumSqrd - sum**2)/real(sampleSize, &
        kind=writeRK)

    ! Print elapsed time.
    if (sampleSize > 1) then
      print "(/a, f12.3, a)", "Average time: ", meanTime, " ms"
    else
      print "(/a, f12.3, a)", "Elapsed time: ", meanTime, " ms"
    end if

    ! Print the standard deviation.
    if (sampleSize > 1) &
      print "(a, f11.3, a)", "Std deviation: ", stdDevTime, " ms"

    ! Record mean time and std deviation.
    if (toRecordTime) then
      timeWriter = constructWriter([timeFlag])
      call timeWriter%initialize()
      call timeWriter%writeHeader(timeFlag, &
          ["max time step       ", &
           "initial pop size    ", &
           "average elapsed time", &
           "standard deviation  "])
      call timeWriter%write(timeFlag, &
          [real(maxTimeStep, kind=writeRK), &
           real(startingPopSize, kind=writeRK), &
           meanTime, &
           stdDevTime])
      call timeWriter%close()
    end if
  end subroutine multipleRun


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeRunWriter
  !>  Initialize a `Writer` object based on the integer `recordFlag`
  !!  passed.  There are three flags: `popRecFlag`, `demogRecFlag`
  !!  and `death_recflag`.
  ! -------------------------------------------------------------------------- !
  subroutine initializeRunWriter(runWriter, recordFlag)
    implicit none

    type(Writer), intent(inout) :: runWriter
    integer,      intent(in)    :: recordFlag

    runWriter = constructWriter([popFlag, ageDstrbFlag, genomeDstrbFlag, &
        deathFlag])

    select case (recordFlag)
      case (nullRecFlag)
        ! Placeholder. Does nothing.

      case (popRecFlag)
        call runWriter%initialize(popFlag)
        call runWriter%writeHeader(popFlag, ["population size"])

      case (demogRecFlag)
        call runWriter%initialize([ageDstrbFlag, genomeDstrbFlag])
        call runWriter%writeHeader(ageDstrbFlag, ["age =>"])
        call runWriter%writeHeader(genomeDstrbFlag, ["age =>"])

      case (deathRecFlag)
        call runWriter%initialize(deathFlag)
        call runWriter%writeHeader(deathFlag, &
            ["death by old age        ", &
             "death by mutation       ", &
             "death by Verhulst factor"])

      case default
        print "(a, i0, a)", "***ERROR. '", recordFlag, &
            "' is an invalid record flag"
        stop
    end select
  end subroutine initializeRunWriter


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: wrapUp
  !>  Wrap up the simulation. This deallocates allocatable arrays.
  ! -------------------------------------------------------------------------- !
  subroutine wrapUp
    implicit none
    
    call deallocVerhulstWeights()
  end subroutine wrapUp
end module Penna

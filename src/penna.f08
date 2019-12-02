module Penna
  use PersonType
  implicit none
  private

  ! Record flags. TODO: Allow multiple flags.
  integer, parameter, public :: nullFlag = 0      ! Null
  integer, parameter, public :: pop_recFlag = 1   ! Population
  integer, parameter, public :: demog_recFlag = 2 ! Age and genome demographics
  integer, parameter, public :: death_recFlag = 3 ! Death count

  public :: multipleRun
  public :: readModelParam
  public :: wrapUp
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readModelParam
  !>  Wrapper procedure for `readScalarParam
  ! -------------------------------------------------------------------------- !
  subroutine readModelParam
    use ModelParam, only: readScalarParam, readVerhulstWeights
    implicit none

    call readScalarParam
    call readVerhulstWeights
  end subroutine readModelParam


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: run
  !>  Simulate the Penna model.
  ! -------------------------------------------------------------------------- !
  subroutine run(maxTimestep, startPopSize, recordFlag)
    use Pop
    use SaveFormat
    use Demographics
    use ModelParam, only: MODEL_K
    use StdKind, only: writeIntKind
    implicit none

    integer, intent(in) :: maxTimestep
    integer, intent(in) :: startPopSize
    integer, intent(in) :: recordFlag

    type(LinkedList) :: popList = LinkedList() ! Struct containing LL pointers.

    type(Writer) :: runWriter     ! A `Writer` object for recording the run.
    integer      :: step          ! Time step
    integer      :: popSize       ! Current population size
    integer      :: deathCount(3) ! Death count.

    ! Initialize the current population.
    allocate(popList%head_ptr)
    call generatePopulation(popList, startPopSize)
    popList%newTail_ptr => popList%tail_ptr

    ! Initialize variables.
    popSize = startPopSize
    deathCount(:) = 0
    call resetDstrbs  ! Initialize demographics.
    call initializeRunWriter(runWriter, recordFlag)

    ! Disable demographics recording if it is not to be recorded.
    if (recordFlag /= demog_recFlag) DEMOG_LAST_STEPS = -1

    ! Run the simulation.
    mainLoop: do step = 1, maxTimestep

      if (popSize > MODEL_K) then
        print "(/a)", "The population has exceeded the carrying capacity!"
        exit
      end if
      
      ! Evaluate each individuals.
      call evalPopulation(popList, popSize, deathCount, maxTimestep - step)

      ! Record result.
      select case (recordFlag)
        case (pop_recFlag)
          call runWriter%write(popFlag, int(popSize, kind=writeIntKind))
        case (demog_recFlag)
          call runWriter%write(ageDstrbFlag, demog_ageDstrb)
          call runWriter%write(genomeDstrbFlag, demog_genomeDstrb)
        case (death_recFlag)
          call runWriter%write(deathFlag, int(deathCount, kind=writeIntKind))
      end select

      ! Reset variables.
      deathCount(:) = 0
      call resetDstrbs
    end do mainLoop

    ! Wrap up.
    if (associated(popList%head_ptr)) then
      if (popSize > 0) then
        call freeAll(popList%head_ptr)
      else
        ! NOTE: A solution to the double free/corruption error.
        popList%head_ptr => null()
      end if
    end if

    call runWriter%close()
    call deallocDstrb
  end subroutine run


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalPopulation
  !>  Evaluate death and birth of individuals.
  ! -------------------------------------------------------------------------- !
  subroutine evalPopulation(popList, popSize, deathCount, timeCountDown)
    use Pop
    use Demographics
    use Flag
    implicit none

    ! A container for LL node pointers.
    type(LinkedList), intent(inout) :: popList

    integer, intent(inout) :: popSize
    integer, intent(inout) :: deathCount(3)
    integer, intent(in)    :: timeCountDown
    
    type(Person), pointer :: oldIndiv_ptr => null()
    type(Person), pointer :: currIndiv_ptr => null()
    integer               :: popSizeOffset = 0

    ! Initialize variables.
    popSizeOffset = 0
    currIndiv_ptr => popList%head_ptr
    oldIndiv_ptr => null()

    do
      ! Catch case where the population is extinct.
      if (popSize == 0) exit
      
      call checkDeath(currIndiv_ptr, popSize, popSizeOffset)

      ! Evaluate alive individual. Count dead ones.
      select case (currIndiv_ptr%deathIndex)
        case (ALIVE)
          currIndiv_ptr%age = currIndiv_ptr%age + 1
          call checkBirth(currIndiv_ptr, popList%newTail_ptr, popSizeOffset)
        case (DEAD_OLD_AGE)
          deathCount(1) = deathCount(1) + 1
        case (DEAD_MUTATION)
          deathCount(2) = deathCount(2) + 1
        case (DEAD_VERHULST)
          deathCount(3) = deathCount(3) + 1
      end select

      ! Record demographics.
      if (timeCountDown <= DEMOG_LAST_STEPS) then
        call updateAgeDstrb(currIndiv_ptr%age, demog_ageDstrb)
        call updateGenomeDstrb(currIndiv_ptr%genome, demog_genomeDstrb)
      end if

      ! Exit condition.
      ! ***Terminal case: end of the population linked-list.
      if (associated(currIndiv_ptr, popList%tail_ptr)) then
        ! Remove dead individual from the list.
        if (currIndiv_ptr%deathIndex /= ALIVE) then
          call killIndiv(currIndiv_ptr, oldIndiv_ptr)

          ! Check edge case.
          if (associated(currIndiv_ptr)) then
            popList%tail_ptr => currIndiv_ptr
          else
            currIndiv_ptr => oldIndiv_ptr
            popList%tail_ptr => currIndiv_ptr
            popList%newTail_ptr => currIndiv_ptr
          end if
        end if
        exit

      ! ***Non-terminal case.
      else
        ! Move to the next individual.
        if (currIndiv_ptr%deathIndex == ALIVE) then
          oldIndiv_ptr => currIndiv_ptr
          currIndiv_ptr => currIndiv_ptr%next

        ! Remove dead individual and move to the next individual.
        else
          ! Check edge case.
          if (associated(currIndiv_ptr, popList%head_ptr)) &
              popList%head_ptr => currIndiv_ptr%next

          call killIndiv(currIndiv_ptr, oldIndiv_ptr)
        end if
      end if
    end do

    ! Update population size.
    popSize = popSize + popSizeOffset

    ! Reset pointers.
    if (popSize > 0) then
      popList%tail_ptr => popList%newTail_ptr
    else
      if (associated(popList%tail_ptr)) popList%tail_ptr => null()
      if (associated(popList%head_ptr)) popList%head_ptr => null()
    end if
  end subroutine evalPopulation


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeAll
  !>  Free remaining allocated memory to prevent memory leak.
  ! -------------------------------------------------------------------------- !
  subroutine freeAll(head_ptr)
    implicit none
    type(Person), pointer, intent(inout) :: head_ptr
    type(Person), pointer                :: curr_ptr => null()
    type(Person), pointer                :: next_ptr => null()

    curr_ptr => head_ptr
    next_ptr => null()
    do
      if (associated(curr_ptr)) then
        next_ptr => curr_ptr%next
        deallocate(curr_ptr)
        curr_ptr => next_ptr
      else
        exit
      end if
    end do
  end subroutine freeAll


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: multipleRun
  !>  Call the `run` subroutine and time it for `sampleSize` times.
  ! -------------------------------------------------------------------------- !
  subroutine multipleRun(maxTimeStep, startingPopSize, sampleSize, recordFlag)
    use SaveFormat
    use TickerType
    implicit none

    integer, intent(in) :: maxTimeStep
    integer, intent(in) :: sampleSize   
    integer, intent(in) :: startingPopSize
    integer, intent(in) :: recordFlag

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
    integer :: j

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
      call runTicker%incrementTick
      write(*, "(*(a))", advance="no") (char(8), j = 1, 10)
      call runTicker%showTicker
      write(*, "(f6.1, a)", advance="no") 100*real(i)/real(sampleSize), "%"
    end do

    ! Get average elapsed time and its std deviation.
    meanTime = sum/real(sampleSize, kind=writeRK)
    stdDevTime = sqrt(sampleSize*sumSqrd - sum**2)/real(sampleSize, &
        kind=writeRK)
    ! Print time statistics.
    print "(/a, f12.3, a)", "Average time: ", meanTime, " ms"
    if (sampleSize > 1) then
      print "(a, f11.3, a)", "Std deviation: ", stdDevTime, " ms"
    end if

    ! Record mean time.
    timeWriter = constructWriter([timeFlag])
    call timeWriter%initialize
    call timeWriter%write(timeFlag, [real(maxTimeStep, kind=writeRK), &
        real(startingPopSize, kind=writeRK), meanTime, stdDevTime])
    call timeWriter%close
  end subroutine multipleRun


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeRunWriter
  !>  Initialize a `Writer` object based on the integer `recordFlag`
  !!  passed.  There are three flags: `pop_recFlag`, `demog_recFlag`
  !!  and `death_recflag`.
  ! -------------------------------------------------------------------------- !
  subroutine initializeRunWriter(runWriter, recordFlag)
    use SaveFormat
    implicit none

    type(Writer), intent(inout) :: runWriter
    integer,      intent(in)    :: recordFlag

    runWriter = constructWriter([popFlag, ageDstrbFlag, genomeDstrbFlag, &
        deathFlag])

    select case (recordFlag)
      case (pop_recFlag)
        call runWriter%initialize(popFlag)
      case (demog_recFlag)
        call runWriter%initialize([ageDstrbFlag, genomeDstrbFlag])
      case (death_recFlag)
        call runWriter%initialize(deathFlag)
      case (nullFlag)
        ! Placeholder. Does nothing.
      case default
        print "(a, i0, a)", "***Warning. '", recordFlag, &
            "' is an invalid record flag. Defaulting to 0 (record nothing)."
    end select
  end subroutine initializeRunWriter

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: wrapUp
  !>  Wrap up the simulation. This deallocates allocatable arrays.
  ! -------------------------------------------------------------------------- !
  subroutine wrapUp
    use ModelParam, only: deallocVerhulstWeights
    implicit none
    
    call deallocVerhulstWeights
  end subroutine wrapUp
end module Penna

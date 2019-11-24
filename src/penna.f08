module Penna
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
    use Model, only: readScalarParam, readVerhulstWeights
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
    use PersonType
    use Demographics
    use Model, only: MODEL_K
    use StdKind, only: writeIntKind
    implicit none

    integer, intent(in) :: maxTimestep
    integer, intent(in) :: startPopSize
    integer, intent(in) :: recordFlag

    type(Person), pointer :: popHead_ptr => null()
    type(Person), pointer :: popTail_ptr  => null()
    type(Person), pointer :: popFutureTail_ptr  => null()

    type(Writer) :: runWriter     ! A `Writer` object for recording the run.
    integer      :: popSize       ! Current population size
    integer      :: step          ! Time step
    integer      :: indexOffset   ! Offset due to deaths and births

    ! Initialize the current population.
    allocate(popHead_ptr)
    call generatePopulation(popHead_ptr, popTail_ptr, startPopSize)
    popFutureTail_ptr => popTail_ptr

    ! Initialize variables.
    popSize = startPopSize
    indexOffset = 0
    call resetDstrbs  ! Initialize demographics.
    call initializeRunWriter(runWriter, recordFlag)

    ! === MAIN LOOP ===
    do step = 1, maxTimestep

      if (popSize > MODEL_K) then
        print "(a)", "The population has exceeded the carrying capacity!"
        exit
      end if
      
      ! Evaluate each individuals.
      call evalPopulation(popHead_ptr, popTail_ptr, popFutureTail_ptr, popSize,&
          indexOffset, step, recordFlag)

      ! Update population size.
      popSize = popSize + indexOffset

      ! Record population size and age demographics.
      ! NOTE: I used select case here to make this faster than the older
      ! implementation.
      select case (recordFlag)
        case (popFlag)
          call runWriter%write(popFlag, int(popSize, kind=writeIntKind))
        case (ageDstrbFlag)
          call runWriter%write(ageDstrbFlag, demog_ageDstrb)
        case (genomeDstrbFlag)
          call runWriter%write(genomeDstrbFlag, demog_genomeDstrb)
      end select

      ! Reset variables.
      indexOffset = 0
      call resetDstrbs
    end do
    ! === MAIN LOOP END ===

    ! Wrap up.
    call freeAll(popHead_ptr)
    call runWriter%close
    call deallocDstrb
  end subroutine run


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalPopulation
  !>  Evaluate death and birth of individuals.
  ! -------------------------------------------------------------------------- !
  subroutine evalPopulation(popHead_ptr, popTail_ptr, popFutureTail_ptr, &
        popSize, indexOffset, timeStep, recordFlag)
    use Pop
    use PersonType
    use Demographics
    use Flag, only: ALIVE
    implicit none

    type(Person), pointer, intent(inout) :: popHead_ptr
    type(Person), pointer, intent(inout) :: popTail_ptr
    type(Person), pointer, intent(inout) :: popFutureTail_ptr
    
    integer, intent(inout) :: popSize
    integer, intent(inout) :: indexOffset
    integer, intent(in)    :: timeStep
    integer, intent(in)    :: recordFlag
    
    type(Person), pointer :: oldIndiv_ptr => null()
    type(Person), pointer :: currIndiv_ptr => null()
    integer,      save    :: demogStep = 0

    currIndiv_ptr => popHead_ptr
    oldIndiv_ptr => null()
    do
      ! Catch case where the population is extinct.
      if (popSize == 0) exit
      
      call checkDeath(currIndiv_ptr, popSize, indexOffset)

      ! Evaluate alive individual.
      if (currIndiv_ptr%deathIndex == ALIVE) then
        currIndiv_ptr%age = currIndiv_ptr%age + 1
        call checkBirth(currIndiv_ptr, popFutureTail_ptr, indexOffset)
      end if

      ! Record demographics.
      if (timeStep <= DEMOG_LAST_STEPS .and. recordFlag == demog_recFlag) then
        demogStep = DEMOG_LAST_STEPS - timeStep + 1
        call updateAgeDstrb(currIndiv_ptr%age, demog_ageDstrb)
        call updateGenomeDstrb(currIndiv_ptr%genome, demog_genomeDstrb)
      end if

      ! Exit condition
      if (associated(currIndiv_ptr, popTail_ptr)) then
        ! Remove dead individual from the list.
        if (currIndiv_ptr%deathIndex /= ALIVE) then
          call killIndiv(currIndiv_ptr, oldIndiv_ptr)

          ! Check edge case.
          if (associated(currIndiv_ptr)) then
            popTail_ptr => currIndiv_ptr
          else
            currIndiv_ptr => oldIndiv_ptr
            popTail_ptr => currIndiv_ptr
            popFutureTail_ptr => currIndiv_ptr
          end if
        end if
        exit

      ! Proceed to the next element of the list.
      else
        ! Move to the next individual.
        if (currIndiv_ptr%deathIndex == ALIVE) then
          oldIndiv_ptr => currIndiv_ptr
          currIndiv_ptr => currIndiv_ptr%next

        ! Remove dead individual and move to the next individual.
        else
          ! Check edge case.
          if (associated(currIndiv_ptr, popHead_ptr)) popHead_ptr => &
              currIndiv_ptr%next

          call killIndiv(currIndiv_ptr, oldIndiv_ptr)
        end if
      end if
    end do

    ! Reset pointers.
    if (popSize > 0) then
      popTail_ptr => popFutureTail_ptr
    else
      if (associated(popTail_ptr)) popTail_ptr => null()
      if (associated(popHead_ptr)) popHead_ptr => null()
    end if
  end subroutine evalPopulation



  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeAll
  !>  Free remaining allocated memory to prevent memory leak.
  ! -------------------------------------------------------------------------- !
  subroutine freeAll(head_ptr)
    use PersonType
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
  subroutine multipleRun(maxTimeStep, startingPopSize, sampleSize, recordFlag, &
        wallTime)
    use StdKind, only: timingIntKind, timingRealKind, writeIntKind
    use SaveFormat
    use TickerType
    implicit none

    integer, intent(in) :: maxTimeStep
    integer, intent(in) :: sampleSize   
    integer, intent(in) :: startingPopSize
    integer, intent(in) :: recordFlag
    real(kind=timingRealKind), intent(out) :: wallTime

    integer(kind=timingIntKind) :: startTimeInt
    integer(kind=timingIntKind) :: endTimeInt
    real(kind=timingRealKind)   :: startTimeReal
    real(kind=timingRealKind)   :: endTimeReal
    real(kind=timingRealKind)   :: clockRate
    real(kind=timingRealKind)   :: sum

    type(Writer) :: timeWriter    ! `Writer` object to write timings stats
    type(Ticker) :: runTicker     ! `Ticker` object for the fancy progress bar
    integer :: i
    integer :: j

    ! Initialize `runTicker`.
    runTicker = constructTicker(20, sampleSize)

    ! Call and time the `run` subroutine.
    sum = 0.
    do i = 1, sampleSize
      ! Start timer.
      call system_clock(count=startTimeInt, count_rate=clockRate)  
      startTimeReal = real(startTimeInt, kind=timingRealKind)/clockRate

      ! Run the actual simulation.
      call run(maxTimeStep, startingPopSize, recordFlag)

      ! End timer.
      call system_clock(count=endTimeInt, count_rate=clockRate)
      endTimeReal = real(endTimeInt, kind=timingRealKind)/clockRate

      sum = sum + (endTimeReal - startTimeReal)*1e3
      call runTicker%incrementTick
      
      ! Print progress bar
      write(*, "(*(a))", advance="no") (char(8), j = 1, 10)
      call runTicker%showTicker
      write(*, "(f6.1, a)", advance="no") 100*real(i)/real(sampleSize), "%"
    end do

    ! Get average wall time.
    wallTime = sum/real(sampleSize, kind=timingRealKind)
    print "(/a, f12.3, a)", "Average time: ", wallTime, " ms"

    ! Record mean time.
    timeWriter = constructWriter([timeFlag])
    call timeWriter%initialize
    call timeWriter%write(timeFlag, [real(maxTimeStep, kind=timingRealKind), &
        real(startingPopSize, kind=timingRealKind), wallTime])
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

    select case(recordFlag)
    case(pop_recFlag)
      call runWriter%initialize(popFlag)
    case(demog_recFlag)
      call runWriter%initialize([ageDstrbFlag, genomeDstrbFlag])
    case(death_recFlag)
      call runWriter%initialize(deathFlag)
    end select
  end subroutine initializeRunWriter

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: wrapUp
  !>  Wrap up the simulation. This deallocates allocatable arrays.
  ! -------------------------------------------------------------------------- !
  subroutine wrapUp
    use Model, only: deallocVerhulstWeights
    implicit none
    
    call deallocVerhulstWeights
  end subroutine wrapUp
end module Penna

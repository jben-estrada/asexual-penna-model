module Penna
  implicit none
  private

  ! Record flags. TODO: Allow multiple flags.
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
  subroutine run(maxTimestep, startPopSize, arraySize, recordFlag)
    use Pop
    use SaveFormat
    use PersonType
    use Demographics
    use Flag, only: ALIVE
    use Model, only: MODEL_K
    use StdKind, only: writeIntKind
    implicit none

    integer, intent(in) :: maxTimestep
    integer, intent(in) :: startPopSize
    integer, intent(in) :: arraySize
    integer, intent(in) :: recordFlag

    type(Person), allocatable :: currPop(:) ! Current population array
    type(Person), allocatable :: nextPop(:) ! Next population array

    type(Writer) :: runWriter     ! A `Writer` object for recording the run.
    integer      :: popSize       ! Current population size
    integer      :: step          ! Time step 
    integer      :: idx           ! Index of individual
    integer      :: indexOffset   ! Offset due to deaths and births
    integer      :: demogStep     ! Index for demographics

    ! Initialize the current population.
    allocate(currPop(arraySize), nextPop(arraySize))
    call generatePopulation(currPop, startPopSize)

    ! Initialize variables.
    popSize = startPopSize
    indexOffset = 0
    call resetDstrbs  ! Initialize demographics.
    call initializeRunWriter(runWriter, recordFlag) ! Initialize writer.

    ! === MAIN LOOP ===
    do step = 1, maxTimestep

      if (popSize > MODEL_K) then
        print "(a)", "The population has exceeded the carrying capacity!"
        exit
      end if

      ! === EVALUATE EACH INDIVIDUALS ===
      do idx = 1, popSize
        call checkDeath(currPop(idx), popSize, indexOffset)

        ! Evaluate the alive ones.
        if (currPop(idx)%deathIndex == ALIVE) then
          currPop(idx)%age = currPop(idx)%age + 1
          call checkBirth(currPop(idx), idx, popSize, nextPop, indexOffset)

          ! Record demographics.
          if (step <= DEMOG_LAST_STEPS .and. recordFlag == demog_recFlag) then
            demogStep = DEMOG_LAST_STEPS - step + 1
            call updateAgeDstrb(currPop(idx)%age, demog_ageDstrb)
            call updateGenomeDstrb(currPop(idx)%genome, demog_genomeDstrb)
          end if

          ! Push the alive ones into the next generation.
          if (step < maxTimestep) then
            nextPop(idx + indexOffset) = currPop(idx)
          end if
        end if
      end do
      ! === EVAL END ===
      ! Update population size.
      popSize = popSize + indexOffset

      ! Record population size and age demographics.
      if (recordFlag /= nullFlag) then
        call runWriter%write(popFlag, int(popSize, kind=writeIntKind))
        call runWriter%write(ageDstrbFlag, demog_ageDstrb)
        call runWriter%write(genomeDstrbFlag, demog_genomeDstrb)
      end if

      ! Reset variables.
      currPop = nextPop
      indexOffset = 0
      call resetDstrbs
    end do
    ! === MAIN LOOP END ===

    ! Wrap up.
    call runWriter%close
    deallocate(currPop, nextPop)
    call deallocDstrb
  end subroutine run


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: multipleRun
  !>  Call the `run` subroutine and time it for `sampleSize` times.
  ! -------------------------------------------------------------------------- !
  subroutine multipleRun(maxTimeStep, startingPopSize, sampleSize, arraySize, &
      recordFlag, wallTime)
      use StdKind, only: timingIntKind, timingRealKind, writeIntKind
    use SaveFormat
    use TickerType
    implicit none

    integer, intent(in) :: maxTimeStep
    integer, intent(in) :: sampleSize   
    integer, intent(in) :: startingPopSize
    integer, intent(in) :: arraySize
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
      call run(maxTimeStep, startingPopSize, arraySize, recordFlag)

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

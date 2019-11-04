program Main
  use Model, only: readIni, readVerhulstWeights, deallocVerhulstWeights
  use StdKind, only: timingIntKind, timingRealKind, writeIntKind
  implicit none
  ! -------------------------------------------------------------------------- !
  ! Arguments to run the simulation.
  integer :: timeSteps
  integer :: sampleSize_ 
  integer :: startPopSize_ 
  integer :: popArrSize
  integer :: recordFlag_
  real(kind=timingRealKind) :: meanTime

  ! Record flags. TODO: Allow multiple flags.
  integer, parameter :: pop_recFlag = 1   ! Record population
  integer, parameter :: demog_recFlag = 2 ! Record age and genome demographics
  integer, parameter :: death_recFlag = 3 ! Record death count

  ! Separator character array for pretty printing.
  integer              :: k  ! Index variable for `separator`
  character, parameter :: separator(27) = [("=", k = 1, 27)]
  ! -------------------------------------------------------------------------- !

  ! Initialize model parameters.
  call readIni
  call readVerhulstWeights

  ! Get command line arguments.
  call getCmdArgs(timeSteps, sampleSize_, startPopSize_, recordFlag_)
  ! Get sizes of population arrays. TODO: Generalize
  popArrSize = getPopArrSize(startPopSize_)

  ! Pretty print cmd arguments and some model parameters.
  call printArgs(timeSteps, sampleSize_, startPopSize_, popArrSize, &
      recordFlag_)

  ! Simulate the Penna model.
  call multipleRun(timeSteps, startPopSize_, sampleSize_, popArrSize, &
      recordFlag_, meanTime)

  ! Wrap up.
  call deallocVerhulstWeights
  print "(*(a))", separator
contains

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

    ! Initialize writer.
    call initializeRunWriter(runWriter, recordFlag)

    ! === MAIN LOOP ===
    do step = 1, maxTimestep

      ! Exit from the loop if the population size exceeds the carrying capacity.
      if (popSize > MODEL_K) then
        print "(a)", "The population has exceeded the carrying capacity!"
        exit
      end if

      ! === EVALUATE EACH INDIVIDUALS ===
      do idx = 1, popSize
        ! Check for death event.
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
      call runTicker%showTicker
    end do

    ! Get average wall time.
    wallTime = sum/real(sampleSize, kind=timingRealKind)
    print "(/a, f10.3, a)", "Average time: ", wallTime, " ms"

    ! Record mean time.
    timeWriter = constructWriter([timeFlag])
    call timeWriter%initialize
    call timeWriter%write(timeFlag, [real(maxTimeStep, kind=timingRealKind), &
        real(startingPopSize, kind=timingRealKind), wallTime])
    call timeWriter%close
  end subroutine multipleRun


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getCmdArgs
  !>  Get command line arguments.
  ! -------------------------------------------------------------------------- !
  subroutine getCmdArgs(maxTimestep, sampleSize, startPopSize, recordFlag)
    use Model, only: MODEL_TIME_STEPS, MODEL_N0
    use SaveFormat, only: nullFlag
    implicit none

    integer, intent(out) :: maxTimestep
    integer, intent(out) :: sampleSize
    integer, intent(out) :: startPopSize
    integer, intent(out) :: recordFlag

    integer :: i, cmdInt, cmdError
    character(len=32) :: cmdArg

    ! Default values for cmd arguments.
    maxTimestep = MODEL_TIME_STEPS
    sampleSize = 1
    startPopSize = MODEL_N0
    recordFlag = nullFlag

    do i = 1, 4
      call get_command_argument(i, cmdArg, status=cmdError)
      if (cmdError /= 0) cycle

      read(cmdArg, *) cmdInt
      select case (i)
      case (1)
        maxTimestep = cmdInt
      case (2)
        sampleSize = cmdInt
      case (3)
        startPopSize = cmdInt
      case (4)
        recordFlag = cmdInt
      end select
    end do
  end subroutine getCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printArgs
  !>  Print various parameters.
  ! -------------------------------------------------------------------------- !
  subroutine printArgs(maxTimestep, sampleSize, startPopSize, arraySize, &
        recordFlag)
    use SaveFormat, only: nullFlag
    implicit none

    integer, intent(in) :: maxTimestep
    integer, intent(in) :: sampleSize
    integer, intent(in) :: startPopSize
    integer, intent(in) :: arraySize
    integer, intent(in) :: recordFlag

    logical :: toRecord

    if (recordFlag /= nullFlag) then
      toRecord = .true.
    else
      toRecord = .false.
    end if

    print "(*(a))", separator 
    print "(a)", "Penna model simulation"
    print "(*(a))", separator 
    print "(3(a20, i7/), a20, i7)", "Number of time steps", maxTimestep, &
        "Sample size", sampleSize, &
        "Starting pop size", startPopSize, &
        "Max pop size", arraySize
    print "(a20, L7)", "Record result", toRecord
    print "(*(a))", separator
  end subroutine printArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getPopArrSize
  !>  Get the size of the population arrays.
  ! TODO: 
  !  Make a procedure that predicts array size base on the model params
  ! -------------------------------------------------------------------------- !
  function getPopArrSize(startPopSize) result(arrSize)
    use Model, only: MODEL_L, MODEL_R
    implicit none

    integer, intent(in) :: startPopSize
    integer             :: arrSize
    arrSize = startPopSize*(MODEL_L/MODEL_R + 1) + 1  ! Added an extra space
  end function getPopArrSize


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
    integer, intent(in)         :: recordFlag

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
end program Main
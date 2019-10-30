program Main
  use Pop
  use Model
  use SaveFormat
  use PersonType
  use Demographics
  use TickerType
  use Flag, only: ALIVE
  use iso_fortran_env, only: real64
  implicit none

  integer :: timeSteps
  integer :: sampleSize_        ! NOTE: Suffixed with `_` so that they won't be 
  integer :: startPopSize_      !       read by the internal procedures.
  integer :: popArrSize
  integer :: recordFlag_
  real(kind=real64) :: meanTime

  ! Record flags. NOTE: This is different from the flags in the
  ! `Saveformat` module!
  integer, parameter :: pop_recFlag = 1
  integer, parameter :: demog_recFlag = 2
  integer, parameter :: death_recFlag = 3

  ! Initialize model parameters
  call readIni
  call readVerhulstWeights
  ! Get command line arguments
  call getCmdArgs(timeSteps, sampleSize_, startPopSize_, recordFlag_)
  ! Get sizes of population arrays.
  popArrSize = getPopArrSize(startPopSize_)   ! TODO

  call printArgs(timeSteps, sampleSize_, startPopSize_, popArrSize, &
      recordFlag_)
  call multipleRun(timeSteps, startPopSize_, sampleSize_, popArrSize, &
      recordFlag_, meanTime)
  
  call deallocVerhulstWeights
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: run
  !>  Run the Penna model simulation.
  ! -------------------------------------------------------------------------- !
  subroutine run(maxTimestep, startPopSize, arraySize, recordFlag)
    implicit none
    integer, intent(in) :: maxTimestep
    integer, intent(in) :: startPopSize
    integer, intent(in) :: arraySize
    integer, intent(in) :: recordFlag

    type(Person), allocatable :: currPop(:) ! Current population array
    type(Person), allocatable :: nextPop(:) ! Next population array

    integer :: popSize          ! Current population size
    integer :: step             ! Time step 
    integer :: idx              ! Index of individual
    integer :: indexOffset      ! Offset due to deaths and births
    integer :: demogStep        ! Index for demographics
    type(Writer) :: runWriter   ! A `Writer` object for recording the run.

    ! Initialize the current population
    allocate(currPop(arraySize), nextPop(arraySize))
    call generatePopulation(currPop, startPopSize)

    ! Initialize variables.
    popSize = startPopSize
    indexOffset = 0
    call resetDstrbs  ! Initialize demographics

    ! Initialize writer
    call initializeRunWriter(runWriter, recordFlag)

    ! === MAIN LOOP ===
    do step = 1, maxTimestep
      ! === EVALUATE EACH INDIVIDUALS ===
      do idx = 1, popSize
        ! Check for death event
        call checkDeath(currPop(idx), popSize, indexOffset)

        ! Evaluate alive indiv
        if (currPop(idx)%deathIndex == ALIVE) then
          currPop(idx)%age = currPop(idx)%age + 1
          call checkBirth(currPop(idx), idx, popSize, nextPop, indexOffset)

          ! Record demographics
          if (step <= DEMOG_LAST_STEPS .and. recordFlag == demog_recFlag) then
            demogStep = DEMOG_LAST_STEPS - step + 1
            call updateAgeDstrb(currPop(idx)%age, demog_ageDstrb)
            call updateGenomeDstrb(currPop(idx)%genome, demog_genomeDstrb)
          end if

          ! Push the alive indiv into the next generation
          if (step < maxTimestep) then
            nextPop(idx + indexOffset) = currPop(idx)
          end if
        end if
      end do
      ! === EVAL END ===
      ! Update population size
      popSize = popSize + indexOffset

      ! Record population size and age demographics
      call runWriter%write(popFlag, popSize)
      call runWriter%write(ageDstrbFlag, demog_ageDstrb)
      call runWriter%write(genomeDstrbFlag, demog_genomeDstrb)

      ! Reset variables
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
    implicit none
    integer, intent(in) :: maxTimeStep
    integer, intent(in) :: sampleSize   
    integer, intent(in) :: startingPopSize
    integer, intent(in) :: arraySize
    integer, intent(in) :: recordFlag
    real(kind=real64), intent(out) :: wallTime

    real(kind=real64) :: startTime
    real(kind=real64) :: endTime
    real(kind=real64) :: sum
    type(Writer) :: timeWriter    ! `Writer` object to write timings stats
    integer :: i

    ! Initialize `runTicker`

    ! Call and time the `run` subroutine
    sum = 0
    do i = 1, sampleSize
      call cpu_time(startTime)
      call run(maxTimeStep, startingPopSize, arraySize, recordFlag)
      call cpu_time(endTime)
      sum = sum + (endTime - startTime)*1e3
    end do

    ! Get average wall time.
    wallTime = sum/sampleSize
    print "(/a, f10.3, a)", "Average time: ", wallTime, " ms"

    ! Record mean time.
    timeWriter = constructWriter([timeFlag])
    call timeWriter%initialize
    call timeWriter%write(timeFlag, [real(maxTimeStep, kind=real64), &
        real(startingPopSize, kind=real64), wallTime])
    call timeWriter%close
  end subroutine multipleRun


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getCmdArgs
  !>  Get command line arguments.
  ! -------------------------------------------------------------------------- !
  subroutine getCmdArgs(maxTimestep, sampleSize, startPopSize, recordFlag)
    implicit none
    integer, intent(out) :: maxTimestep
    integer, intent(out) :: sampleSize
    integer, intent(out) :: startPopSize
    integer, intent(out) :: recordFlag

    integer :: i, cmdInt, cmdError
    character(len=32) :: cmdArg

    ! Default values for cmd arguments
    maxTimestep = MODEL_TIME_STEPS_D
    sampleSize = 1
    startPopSize = MODEL_N0_D
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
    implicit none
    integer, intent(in) :: maxTimestep
    integer, intent(in) :: sampleSize
    integer, intent(in) :: startPopSize
    integer, intent(in) :: arraySize
    integer, intent(in) :: recordFlag

    integer :: i
    logical :: toRecord

    if (recordFlag /= nullFlag) then
      toRecord = .true.
    else
      toRecord = .false.
    end if

    print "(a,/*(a))", "Penna model simulation", ("-", i = 1, 27)
    print "(3(a20, i7/), a20, i7)", "Number of time steps", maxTimestep, &
        "Sample size", sampleSize, &
        "Starting pop size", startPopSize, &
        "Array size", arraySize
    print "(a20, L7/)", "Record result", toRecord
  end subroutine printArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getPopArrSize
  !>  Get the size of the population arrays.
  ! TODO: 
  !  Make a procedure that predicts array size base on the model params
  ! -------------------------------------------------------------------------- !
  function getPopArrSize(startPopSize) result(arrSize)
    implicit none
    integer, intent(in) :: startPopSize
    integer :: arrSize
    arrSize = startPopSize*(MODEL_L/MODEL_R + 1) + 1  ! Added an extra space
  end function getPopArrSize


  subroutine initializeRunWriter(runWriter, recordFlag)
    implicit none
    type(Writer), intent(inout) :: runWriter
    integer, intent(in) :: recordFlag

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
module Penna
  use Pop
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
  !>  Run the Penna model.
  ! -------------------------------------------------------------------------- !
  subroutine run(maxTimestep, startPopSize, recordFlag)
    use Flag
    use SaveFormat
    use Demographics
    use ModelParam
    use StdKind, only: writeIntKind
    implicit none

    integer, intent(in) :: maxTimestep
    integer, intent(in) :: startPopSize
    integer, intent(in) :: recordFlag

    type(LinkedList) :: population
    type(Writer)     :: runWriter
    integer          :: step
    integer          :: popSize
    integer          :: popSizeChange
    integer          :: listStatus
    integer          :: deathCount(3)

    ! Initialize variables
    population = constructLinkedList(startPopSize)
    popSize = startPopSize
    popSizeChange = 0
    deathCount(:) = 0
    listStatus = 0
    call resetDstrbs()
    call initializeRunWriter(runWriter, recordFlag)

    ! Disable demographics recording.
    if (recordFlag /= demog_recFlag) DEMOG_LAST_STEPS = -1

    ! Run the model.
    mainloop: do step = 1, maxTimestep
      if (popSize > MODEL_K) then
        print "(/a)", "The population has exceeded the carrying capacity!"
        exit
      end if

      ! Evaluate population.
      evalPop: do
        ! Catch extinction case.
        if (popSize == 0) exit

        ! Evaluate the current individual.
        if (population%isCurrIndivDead(popSize)) then
          ! Count dead ones.
          select case (population%getCurrIndivDeathIdx())
            case (DEAD_OLD_AGE)
              deathCount(1) = deathCount(1) + 1
            case (DEAD_MUTATION)
              deathCount(2) = deathCount(2) + 1
            case (DEAD_VERHULST)
              deathCount(3) = deathCount(3) + 1
            case default
              error stop "Dead `Person` object has an invalid death index."
          end select
          popSizeChange = popSizeChange - 1
        else
          call population%updateCurrIndivAge()

          ! Check for birth events.
          if (population%isCurrIndivMature()) then
            call population%reproduceCurrIndiv()
            popSizeChange = popSizeChange + MODEL_B
          end if
        end if

        ! Record demographics.
        if (maxTimestep - step <= DEMOG_LAST_STEPS) then
          call updateAgeDstrb(population%getCurrIndivAge(), demog_ageDstrb)
          call updateGenomeDstrb(population%getCurrIndivGenome(), &
              demog_genomeDstrb)
        end if

        ! Proceed to the next element of the linked list.
        call population%nextElem(listStatus)
        ! Exit condition.
        if (listStatus /= 0) exit
      end do evalPop
      ! Update the population size.
      popSize = popSize + popSizeChange

      ! Record result.
      select case (recordFlag)
        case (pop_recFlag)
          call runWriter%write(popFlag, int(popSize, kind=writeIntKind))
        case (demog_recFlag)
          call runWriter%write((ageDstrbFlag), &
              int(demog_ageDstrb, kind=writeIK))
          call runWriter%write(genomeDstrbFlag, &
              int(demog_genomeDstrb, kind=writeIK))
        case (death_recFlag)
          call runWriter%write(deathFlag, int(deathCount, kind=writeIntKind))
      end select

      ! Reset variables.
      deathCount(:) = 0
      popSizeChange = 0
      call population%resetReadPtrs()
      if (recordFlag == demog_recFlag) call resetDstrbs()
    end do mainloop

    ! Wrap up.
    call population%freePtr(popSize)
    call runWriter%close()
    call deallocDstrb()
  end subroutine run


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

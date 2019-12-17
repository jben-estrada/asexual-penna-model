module CmdInterface
  use ModelParam
  use Penna
  implicit none
  private

  integer :: k  ! Index variable for `separator`
  character, public, parameter :: separator(29) = [("=", k = 1, 29)]
  integer,           parameter :: MAX_LEN = 99

  public :: getCmdArgs
  public :: printArgs
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getCmdArgs
  !>  Get command line arguments.
  !   TODO: Would be great if the dummy args are reduced.
  ! -------------------------------------------------------------------------- !
  subroutine getCmdArgs(maxTimestep, sampleSize, startPopSize, recordFlag, &
      rngChoice, rngSeed, isVerbosePrint, toRecordTime)
    use RNG, only: RNG_INTRINSIC
    implicit none

    integer, intent(out) :: maxTimestep
    integer, intent(out) :: sampleSize
    integer, intent(out) :: startPopSize
    integer, intent(out) :: recordFlag
    integer, intent(out) :: rngChoice
    integer, intent(out) :: rngSeed
    logical, intent(out) :: isVerbosePrint
    logical, intent(out) :: toRecordTime

    character(len=32)      :: cmdArg
    integer :: readStatus
    integer :: argCount

    ! Default values for cmd arguments.
    maxTimestep = MODEL_TIME_STEPS
    sampleSize = MODEL_SAMPLE_SIZE
    startPopSize = MODEL_N0
    recordFlag = nullRecFlag
    isVerbosePrint = .false.
    toRecordTime = .false.
    rngChoice = RNG_INTRINSIC
    rngSeed = 1

    ! Read command-line arguments.
    do argCount = 1, command_argument_count()
      call get_command_argument(argCount, cmdArg, status=readStatus)
      if (readStatus == -1) exit

      call toggleSwitchArg(cmdArg, readStatus, isVerbosePrint, toRecordTime)
      if (readStatus == 0) cycle

      call assignKeyValArg(cmdArg, readStatus, rngSeed, rngChoice)
      if (readStatus == 0) cycle

      call assignPosArg(cmdArg, maxTimestep, sampleSize, startPopSize, &
          recordFlag)
    end do
  end subroutine getCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: toggleSwitchArg
  !>  If the provided cmd argument is a valid switch, toggle it.
  ! -------------------------------------------------------------------------- !
  subroutine toggleSwitchArg(arg, status, isVerbosePrint, toRecordTime)
    implicit none

    character(len=*), intent(in)    :: arg
    integer,          intent(out)   :: status
    logical,          intent(inout) :: isVerbosePrint
    logical,          intent(inout) :: toRecordTime

    status = 0
    select case (arg)
      ! ***Print all model parameters.
      case ("-v", "--verbose")
        isVerbosePrint = .true.
      
      ! ***Show the help message and then exit.
      case ("-h", "--help")
        call printHelp()
        stop
      
      ! ***Record mean elapsed time and the standard deviation.
      case ("--record-time")
        toRecordTime = .true.

      case default
        status = 1
    end select
  end subroutine toggleSwitchArg


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignPosArg
  !>  Assign valid positional arguments.
  ! -------------------------------------------------------------------------- !
  subroutine assignPosArg(arg, maxTimeStep, sampleSize, startPopSize, &
        recordFlag)
    implicit none

    character(len=*), intent(in)    :: arg
    integer,          intent(inout) :: maxTimestep
    integer,          intent(inout) :: sampleSize
    integer,          intent(inout) :: startPopSize
    integer,          intent(inout) :: recordFlag

    integer, save :: posArgCount = 1
    integer       :: int
    integer       :: status

    read(arg, *, iostat=status) int
    if (status == 0) then
      select case (posArgCount)
        ! ***Max time step.
        case (1)
          maxTimestep = int
        ! ***Sample size.
        case (2)
          sampleSize = int
        ! ***Starting population size.
        case (3)
          startPopSize = int
        ! ***Record flag.
        case (4)
          recordFlag = int
      end select

      posArgCount = posArgCount + 1
    else
      print "(3(a))", "***ERROR. '", trim(arg) ,"' is not a valid option. " // &
                "Try 'penna.out -h' for more information."
      stop
    end if
  end subroutine assignPosArg


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignKeyValArg
  !>  Assign valid key-value arguments. 
  ! -------------------------------------------------------------------------- !
  subroutine assignKeyValArg(arg, status, rngSeed, rngChoice)
    use RNG, only: RNG_FLAGS
    implicit none

    character(len=*), intent(in)    :: arg
    integer,          intent(out)   :: status
    integer,          intent(inout) :: rngSeed
    integer,          intent(inout) :: rngChoice

    character(len=MAX_LEN) :: key
    character(len=MAX_LEN) :: valStr
    integer :: val

    call parseKeyValArg(arg, key, valStr, status)

    if (status == 0) then
      read(valStr, *, iostat=status) val

      if (status == 0) then
        select case (key)
          ! ***RNG seed.
          case ("seed")
            ! Check if the provided seed is positive.
            if (val > 0) then
              rngSeed = val
            else
              print "(a)", "***ERROR. RNG seed must be a positive integer."
              stop 
            end if
            
          ! ***RNG choice.
          case ("rng")
            if (any(RNG_FLAGS == val)) then
              rngChoice = val
            else
              print "(a)", "***ERROR. Invalid RNG flag. Try 'penna.out -h' "// &
              "to check for the available RNGs and their corresponding " // &
              "integer flags."
              stop
            end if

          case default
            status = 1
        end select
      end if
    end if
  end subroutine assignKeyValArg


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseKeyValArg
  !>  Read and parse key-value arguments.
  ! -------------------------------------------------------------------------- !
  subroutine parseKeyValArg(arg, key, val, status)
    implicit none

    character(len=*),       intent(in)  :: arg
    character(len=MAX_LEN), intent(out) :: key
    character(len=MAX_LEN), intent(out) :: val
    integer,                intent(out) :: status

    character :: char
    integer   :: i
    logical   :: isReadingKey

    character(len=:), allocatable :: keyTemp
    character(len=:), allocatable :: valTemp

    key = ""
    val = ""
    allocate(character(len=0) :: keyTemp)
    allocate(character(len=0) :: valTemp)
    status = 1
    isReadingKey = .true.

    ! Filter out invalid key-arg arguments.
    if (len(arg) < 1) return
    if (arg(1:1) /= "-") return

    do i = 2, len(arg)
      char = arg(i:i)
      
      ! Shift from LHS to RHS
      if (char == "=") then
        isReadingKey = .false.
        cycle
      end if

      ! Read key/val.
      if (isReadingKey) then
        keyTemp = keyTemp // char
      else
        valTemp = valTemp // char
      end if
    end do

    ! Parsing failed.
    if (isReadingKey) then
      key = ""
      val = ""
      status = 1
    ! Parsing succeed.
    else
      key = keyTemp
      val = valTemp
      status = 0
    end if

    if (allocated(keyTemp)) deallocate(keyTemp)
    if (allocated(valTemp)) deallocate(valTemp)
  end subroutine parseKeyValArg


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printHelp
  !>  Print help or usage message before stopping the program.
  ! -------------------------------------------------------------------------- !
  subroutine printHelp
    use RNG, only: RNG_FLAGS
    implicit none

    character(len=3) :: flagStr(4) 
    character(len=3) :: rngStr(2) 
    integer :: flagArr(4) = &
      [nullRecFlag, popRecFlag, demogRecFlag, deathRecFlag]
    integer :: i

    ! Cast record flag integers to strings.
    do i = 1, size(flagStr)
      write(flagStr(i), "(i2)") flagArr(i)
    end do

    do i = 1, size(rngStr)
      write(rngStr(i), "(i2)") RNG_FLAGS(i)
    end do

    ! TODO: Better help message. Could be made such that it is not hard-coded.
    ! ***Usage message.
    print "(a, /a)", "usage: penna.out [-v | --verbose] [max-time-step] " // &
      "[sample-size] [start-pop-size] [record-flag]", &
      "       penna.out [-h | --help]"

    ! ***Options message.
    print "(/a, *(/7(' '), 2(a)))", "options:", &
      adjustl("-h, --help      "), adjustl("Show this message."), &
      adjustl("-v, --verbose   "), adjustl("Show all the model parameters."), &
      adjustl("--record-time   "), adjustl("Record the average elapsed " // &
          "time and the standard deviation."), &
      adjustl("-seed [int]     "), adjustl("Set seed for the RNG to be " // &
          "used. Must be a positive integer. [default: 1]"), &
      adjustl("-rng [int]      "), adjustl("Choose RNG to be used. " // &
          " [default: 0 (intrinsic RNG)]")

    ! ***Optional parameters.
    print "(/a/, *(7(' '), 2(a), i0, a/))", "optional parameters:", &
      adjustl("max-time-step   "), adjustl("Maximum time step. [default: "), &
          MODEL_TIME_STEPS, "]", &
      adjustl("start-pop-size  "), adjustl("Starting population size. " // &
          "[default: "), MODEL_N0, "]", &
      adjustl("record-flag     "), adjustl("Record specified data. " // &
          "[default: "), 0, "]"
          
    ! ***Notes.
    write(*, "(a/, 7(' '), a/, 4(9(' '), 2(a)/))", advance="no") "notes:", &
        adjustl("- Record flags are as follows:"), &
        flagStr(1), adjustl("- Do not record."), &
        flagStr(2), adjustl("- Record the population size per unit time."), &
        flagStr(3), adjustl("- Record the average demographics of the " // &
            "last 300 steps."), &
        flagStr(4), adjustl("- Record death count.")
    write(*, "(7(' '), a/, 2(9(' '), 2(a)/))") "- RNG flags and their "// &
        "corresponding RNGs are as follows:", &
        rngStr(1), adjustl("- a KISS pseudo-random number generator " // &
            "(the intrinsic RNG)"), &
        rngStr(2), adjustl("- MT19937, a Mersenne Twister pseudo-random " // &
            "number generator.")
  end subroutine printHelp


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printArgs
  !>  Print various parameters.
  ! -------------------------------------------------------------------------- !
  subroutine printArgs(maxTimestep, sampleSize, startPopSize, recordFlag, & 
      isVerbosePrint)
    implicit none

    integer, intent(in) :: maxTimestep
    integer, intent(in) :: sampleSize
    integer, intent(in) :: startPopSize
    integer, intent(in) :: recordFlag
    logical, intent(in) :: isVerbosePrint

    logical :: toRecord
    toRecord = recordFlag /= nullRecFlag

    ! ***Header
    print "(*(a))", separator 
    print "(a)", "Asexual Penna model"
    print "(*(a))", separator

    ! ***Body (Extended model parameters)
    if (isVerbosePrint) call printModelParams

    ! ***Body
    print "(2(a20, i9/), a20, i9)", &
      "Number of time steps", maxTimestep, &
      "Sample size", sampleSize, &
      "Starting pop size", startPopSize
    print "(a20, L9)", "Record result", toRecord

    ! ***End
    print "(*(a))", separator
  end subroutine printArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printModelParams
  !>  Print extended model parameters.
  ! -------------------------------------------------------------------------- !
  subroutine printModelParams
    implicit none
    print "(6(a20, i9/), a20, i9)", &
      "Genome length",        MODEL_L, &
      "Mutation threshold",   MODEL_T, &
      "Birth rate",           MODEL_B, &
      "Mutation rate",        MODEL_M, &
      "Min reproduciton age", MODEL_R, &
      "Max reproduction age", MODEL_R_MAX, &
      "Carrying capacity",    MODEL_K
  end subroutine printModelParams
end module CmdInterface

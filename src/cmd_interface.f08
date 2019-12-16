module CmdInterface
  use ModelParam
  use Penna
  implicit none
  private

  integer :: k  ! Index variable for `separator`
  character, public, parameter :: separator(29) = [("=", k = 1, 29)]

  public :: getCmdArgs
  public :: printArgs
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getCmdArgs
  !>  Get command line arguments.
  !   TODO: Would be great if the dummy args are reduced.
  ! -------------------------------------------------------------------------- !
  subroutine getCmdArgs(maxTimestep, sampleSize, startPopSize, recordFlag, &
      rngSeed, isVerbosePrint, toRecordTime)
  implicit none

  integer, intent(out) :: maxTimestep
  integer, intent(out) :: sampleSize
  integer, intent(out) :: startPopSize
  integer, intent(out) :: recordFlag
  integer, intent(out) :: rngSeed
  logical, intent(out) :: isVerbosePrint
  logical, intent(out) :: toRecordTime

  character(len=32) :: cmdArg
  integer :: cmdInt
  integer :: readStatus
  integer :: argCount
  integer :: posArgCount

  ! Default values for cmd arguments.
  maxTimestep = MODEL_TIME_STEPS
  sampleSize = MODEL_SAMPLE_SIZE
  startPopSize = MODEL_N0
  recordFlag = nullRecFlag
  isVerbosePrint = .false.
  toRecordTime = .false.
  rngSeed = 1

  ! Evaluate each passed cmd args.
  posArgCount = 1
  do argCount = 1, command_argument_count()
    call get_command_argument(argCount, cmdArg, status=readStatus)
    if (readStatus == -1) exit

    select case(cmdArg)
      ! ***Verbose parameter print.
      case ("-v", "--verbose")
        isVerbosePrint = .true.

      ! ***Print help message.
      case ("-h", "--help")
        call printHelp()
        call wrapUp()
        stop
      
      ! *** Record mean elapsed time and its std deviation.
      case ("--record-time")
        toRecordTime = .true.
      
      ! ***Accept positional parameters.
      case default
        read(cmdArg, *, iostat=readStatus) cmdInt

        ! Assign casted value if type casting succeeds
        if (readStatus == 0) then
          select case (posArgCount)
            ! ***Max time step.
            case (1)
              maxTimestep = cmdInt
            ! ***Sample size.
            case (2)
              sampleSize = cmdInt
            ! ***Starting population size.
            case (3)
              startPopSize = cmdInt
            ! ***Record flag.
            case (4)
              recordFlag = cmdInt
          end select
        else
          print "(3(a))", "***Error. '", trim(cmdArg) ,"' is not a valid " // &
              " option. Try 'penna.out -h' for more information."
          call wrapUp
          stop
        end if

        posArgCount = posArgCount + 1
    end select
  end do
  end subroutine getCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printHelp
  !>  Print help or usage message before stopping the program.
  ! -------------------------------------------------------------------------- !
  subroutine printHelp
  implicit none

  character(len=3) :: flagStr(4)    
  integer :: flagArr(4) = &
    [nullRecFlag, popRecFlag, demogRecFlag, deathRecFlag]
  integer :: i

  ! Cast record flag integers to strings.
  do i = 1, size(flagStr)
  write(flagStr(i), "(i2)") flagArr(i)
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
    adjustl("--record-time   "), adjustl("Record the average elapsed time " // &
        "and the standard deviation.")

  ! ***Optional parameters.
  print "(/a/, *(7(' '), 2(a), i0, a/))", "optional parameters:", &
    adjustl("max-time-step   "), adjustl("Maximum time step. [default: "), &
        MODEL_TIME_STEPS, "]", &
    adjustl("start-pop-size  "), adjustl("Starting population size. " // &
        "[default: "), MODEL_N0, "]", &
    adjustl("record-flag     "), adjustl("Record specified data. " // &
        "[default: "), 0, "]"
        
  ! ***Notes.
  write (*, "(a/, 7(' '), a/, 4(9(' '), 2(a)/), a)", advance="no") "notes:", &
    adjustl("- Record flags are as follows:"), &
    flagStr(1), adjustl("- Do not record."), &
    flagStr(2), adjustl("- Record the population size per unit time."), &
    flagStr(3), adjustl("- Record the average demographics of the " // &
        "last 300 steps."), &
    flagStr(4), adjustl("- Record death count."), ""
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
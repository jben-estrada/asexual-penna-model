module CmdOptions
  use CmdOptionType
  implicit none
  private

  ! Flag options.
  type(FlagCmdOption), target :: cmdFlags(4)
  type(FlagCmdOption), pointer, protected, public :: verbosePrintFlag => &
    cmdFlags(1)
  type(FlagCmdOption), pointer, protected, public :: showHelpMsgFlag => &
    cmdFlags(2)
  type(FlagCmdOption), pointer, protected, public :: recordTimeFlag => &
    cmdFlags(3)
  type(FlagCmdOption), pointer, protected, public :: silentPrintFlag => &
    cmdFlags(4)

  ! Key-value arguments: Model parameters.
  type(KeyValCmdOption), target :: cmdKeyVal(6)
  type(KeyValCmdOption), pointer, protected, public :: maxTimeStepArg => &
    cmdKeyVal(1)
  type(KeyValCmdOption), pointer, protected, public :: sampleSizeArg => &
    cmdKeyVal(2)
  type(KeyValCmdOption), pointer, protected, public :: startPopSizeArg => &
    cmdKeyVal(3)

  ! Key-value arguments: Other program parameters.
  type(KeyValCmdOption), pointer, protected, public :: recordFlagArg => &
    cmdKeyVal(4)
  type(KeyValCmdOption), pointer, protected, public :: rngChoiceArg => &
    cmdKeyVal(5)
  type(KeyValCmdOption), pointer, protected, public :: rngSeedArg => &
    cmdKeyVal(6)

  ! Positional arguments.
  type(PositionalCmdOption), target :: cmdPosArgs(2)
  type(PositionalCmdOption), pointer, protected, public :: configDirPosArg => &
    cmdPosArgs(1)
  type(PositionalCmdOption), pointer, protected, public :: vWeightDirPosArg => &
    cmdPosArgs(2)

  public :: initializeCmdOptions
  public :: parseCmdArgs
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeCmdOptions
  !>  Initialize command-line option types.
  ! -------------------------------------------------------------------------- !
  subroutine initializeCmdOptions()
    use ModelParam
    implicit none

    ! Assign command char of flag options.
    call verbosePrintFlag % setCommand("-v", "--verbose")
    call showHelpMsgFlag % setCommand("-h", "--help")
    call recordTimeFlag % setCommand("-r", "--record-time")
    call silentPrintFlag % setCommand("-s", "--silent")
  
    ! Assign command char of key-value options.
    call maxTimeStepArg % setCommand("-max-time-step")
    call sampleSizeArg % setCommand("-sample-size")
    call startPopSizeArg % setCommand("-pop-size")
    call recordFlagArg % setCommand("-rec-flag")
    call rngChoiceArg % setCommand("-rng")
    call rngSeedArg % setCommand("-seed")

    ! Assign commansd char and position of position arguments.
    call configDirPosArg % setCommand("config-directory")
    call configDirPosArg % setPosition(1)
    call vWeightDirPosArg % setCommand("vweight-directory")
    call vWeightDirPosArg % setPosition(2)

    ! Set usage messages of flag options.
    call verbosePrintFlag % setUsageMsg("Show all the model parameters.")
    call showHelpMsgFlag % setUsageMsg("Show this message and exit.")
    call recordTimeFlag % setUsageMsg("Record the average elapsed time " // &
      "of the simulation.")
    call silentPrintFlag % setUsageMsg("Do not show the model parameters.")

    ! Set usage messages of key-value options.
    call maxTimeStepArg % setUsageMsg("Maximum time step. " // &
        getDefaultValStr(MODEL_TIME_STEPS))
    call sampleSizeArg % setUsageMsg("Sample size. "// &
        getDefaultValStr(MODEL_SAMPLE_SIZE))
    call startPopSizeArg % setUsageMsg("Starting population size. " // &
        getDefaultValStr(MODEL_N0))
    call recordFlagArg % setUsageMsg("Record data specified by the " // &
        "given integer flag. " // getDefaultValStr(MODEL_REC_FLAG))
    call rngChoiceArg % setUsageMsg("Choose a random number " // &
        "generator (RNG) to be used as specified by the given" // &
        " integer flag. " // getDefaultValStr(MODEL_RNG))
    call rngSeedArg % setUsageMsg("Set the seed for the RNG." // &
        getDefaultValStr(MODEL_RNG_SEED))

    ! Set usage messages of positional arguments.
    call configDirPosArg % setUsageMsg("Directory of .cfg file of " // &
        "model parameters.")
    call vWeightDirPosArg % setUsageMsg("Directory of .cfg file of " // &
        "Verhulst weights.")
  end subroutine initializeCmdOptions


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getDefaultValStr
  !>  Get default value char for the cmd-line option usage messages.
  ! -------------------------------------------------------------------------- !
  function getDefaultValStr(arg) result(out)
    implicit none
    integer, intent(in) :: arg

    character(len=MAX_LEN)        :: buffer_str
    character(len=:), allocatable :: out

    write(buffer_str, "('[default = ', i0, ']')") arg
    out = trim(buffer_str)
  end function getDefaultValStr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseCmdArgs
  !>  Parse command-line arguments.
  ! -------------------------------------------------------------------------- !
  subroutine parseCmdArgs()
    implicit none

    integer :: argCount
    integer :: status
    character(len=MAX_LEN) :: cmdArg

    do argCount = 1, command_argument_count()
      call get_command_argument(argCount, cmdArg, status=status)

      if (cmdArg == NULL_CHAR) cycle

      call toggleFlagOptions(cmdArg, status)
      if (status == 0) cycle

      call assignKeyValOption(cmdArg, status)
      if (status == 0) cycle

      call assignPositionalArg(cmdArg, status)
      if (status /= 0) then
        print "(3a)", "***ERROR. '", trim(cmdArg), "' is not a valid option."
        stop
      end if
    end do
  end subroutine parseCmdArgs


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: compareCommand
  !>  Compare command and alternative command with the passed command-line
  !!  argument.
  ! -------------------------------------------------------------------------- !
  logical function compareCommand(cmdOption, cmdArg)
    implicit none

    class(BaseCmdOption), intent(in) :: cmdOption
    character(len=*),     intent(in) :: cmdArg

    compareCommand = cmdOption % getCommand() == trim(cmdArg) &
        .or. cmdOption % getAltCommand() == trim(cmdArg)
  end function compareCommand


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: toggleFlagOptions
  !>  Toggle the flag option matching with the passed command-line argument.
  ! -------------------------------------------------------------------------- !
  subroutine toggleFlagOptions(cmdArg, status)
    implicit none

    character(len=*), intent(in)  :: cmdArg
    integer,          intent(out) :: status

    integer :: i

    ! NOTE: Non-zero values for 'status' mean the routine failed.
    status = 1

    do i = 1, size(cmdFlags)
      if (compareCommand(cmdFlags(i), cmdArg)) then
        call cmdFlags(i) % toggle()
        status = 0
        exit
      end if
    end do
  end subroutine toggleFlagOptions


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignKeyValOption
  !>  Assign the value for the matching key-value option.
  ! -------------------------------------------------------------------------- !
  subroutine assignKeyValOption(cmdArg, status)
    implicit none

    character(len=*), intent(in)  :: cmdArg
    integer,          intent(out) :: status

    character(len=MAX_LEN) :: key
    character(len=MAX_LEN) :: valueChar
    integer :: i

    ! Get key and value from 'cmdArg' char.
    call getKeyVal(cmdArg, key, valueChar, status)
    if (status /= 0) return
    
    status = 1
    do i = 1, size(cmdKeyVal)
      if (compareCommand(cmdKeyVal(i), key)) then
        call assignValueTo(cmdKeyVal(i), valueChar)
        status = 0
        exit
      end if
    end do
  end subroutine assignKeyValOption


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignValueTo
  !>  Assign value to the specified key-value option.
  ! -------------------------------------------------------------------------- !
  subroutine assignValueTo(cmdOption, valueChar)
    implicit none
    
    class(KeyValCmdOption), intent(inout) :: cmdOption
    character(len=*),       intent(in)    :: valueChar

    integer :: valueInt
    integer :: status

    read(valueChar, *, iostat=status) valueInt

    if (status == 0) then
      call cmdOption % setValue(valueInt)
    else
      print "(3a)", "***ERROR. '", cmdOption % getCommand(), &
          "' only accepts integers."
      stop
    end if
  end subroutine assignValueTo


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignPositionalArg
  !>  Assign positional arguments.
  ! -------------------------------------------------------------------------- !
  subroutine assignPositionalArg(cmdArg, status)
    implicit none

    character(len=*), intent(in)  :: cmdArg
    integer,          intent(out) :: status

    integer, save :: posCount = 1
    integer :: i

    status = 1
    do i = 1, size(cmdPosArgs)
      if (cmdPosArgs(i) % getPosition() == posCount) then
        call cmdPosArgs(i) % setValue(cmdArg)
        posCount = posCount + 1
        status = 0
        exit
      end if
    end do
  end subroutine assignPositionalArg
end module CmdOptions

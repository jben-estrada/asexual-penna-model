submodule (CmdOptionType) InterfaceProcedures
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseCmdArgs
  !>  Parse command-line arguments.
  ! -------------------------------------------------------------------------- !
  subroutine parseCmdArgs(cmdFlags, cmdKeyVal, cmdPosArgs)
    implicit none

    class(FlagCmdOption),       intent(inout) :: cmdFlags(:)
    class(KeyValCmdOption),     intent(inout) :: cmdKeyVal(:)
    class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)

    integer :: argCount
    integer :: status
    character(len=MAX_LEN) :: cmdArg

    do argCount = 1, command_argument_count()
      call get_command_argument(argCount, cmdArg, status=status)

      if (cmdArg == NULL_CHAR) cycle

      call toggleFlagOptions(cmdFlags, cmdArg, status)
      if (status == 0) cycle

      call assignKeyValOption(cmdKeyVal, cmdArg, status)
      if (status == 0) cycle

      call assignPositionalArg(cmdPosArgs, cmdArg, status)
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
  subroutine toggleFlagOptions(cmdFlags, cmdArg, status)
    implicit none

    class(FlagCmdOption), intent(inout) :: cmdFlags(:)
    character(len=*),     intent(in)    :: cmdArg
    integer,              intent(out)   :: status

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
  subroutine assignKeyValOption(cmdKeyVal, cmdArg, status)
    implicit none
  
    class(KeyValCmdOption), intent(inout) :: cmdKeyVal(:)
    character(len=*),       intent(in)  :: cmdArg
    integer,                intent(out) :: status

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
  subroutine assignPositionalArg(cmdPosArgs, cmdArg, status)
    implicit none

    class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
    character(len=*),           intent(in)    :: cmdArg
    integer,                    intent(out)   :: status

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


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getKeyVal
  !>  Get the key and value characters from the provided cmd argument.
  ! -------------------------------------------------------------------------- !
  subroutine getKeyVal(cmdArg, key, value, status)
    implicit none

    character(len=*),       intent(in)  :: cmdArg
    character(len=MAX_LEN), intent(out) :: key
    character(len=MAX_LEN), intent(out) :: value
    integer,                intent(out) :: status

    integer   :: i
    character :: currChar
    logical   :: isReadingKey
    character(len=:), allocatable :: tempStr

    ! Initialize outputs.
    ! NOTE: Non-zero values for status mean this routine fails.
    status = 0
    key = NULL_CHAR
    value = NULL_CHAR

    ! Initialize local variables.
    isReadingKey = .true.
    allocate(character(len=0) :: tempStr)

    do i = 1, len(cmdArg)
      currChar = cmdArg(i:i)

      if (currChar == KEY_VAL_SEP) then
        isReadingKey = .false.
        key = tempStr
        tempStr = NULL_CHAR
      else
        tempStr = tempStr // currChar
      end if
    end do

    if (isReadingKey) then
      ! Mark the routine "failed" since no "=" is detected.
      status = 1
    else
      ! Get the value string.
      value = tempStr
    end if
    if (allocated(tempStr)) deallocate(tempStr)
  end subroutine getKeyVal
end submodule
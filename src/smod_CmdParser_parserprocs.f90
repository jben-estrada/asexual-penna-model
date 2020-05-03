submodule (CmdArgParserType) parserProcedures
! -------------------------------------------------------------------------- !
  ! SUBMODULE: parserProcedures
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `CmdArgParserType` containing procedures for parsing
  !!  command arguments.
  ! -------------------------------------------------------------------------- !
  implicit none
  
  integer, parameter :: MAX_ARG_LEN = 64
    !! Maximum length of command-line argument.
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseCmdArgs
  !>  Parse command arguments.
  ! -------------------------------------------------------------------------- !
  subroutine parseCmdArgs(parserObj)
    class(CmdArgParser), intent(inout) :: parserObj
      !! `CmdArgParser` object.

    character(len=MAX_ARG_LEN) :: rawChar   ! Raw command argument.
    character(len=:), allocatable :: cmdArg ! Cleaned command argument.

    integer :: cmdIdx     ! Command-line argument position/index
    integer :: getCmdStat ! Status of `get_command_argument` routine.

    ! Initialize local variables.
    allocate(character(len=0) :: cmdArg)
    cmdIdx = 1

    do
      call get_command_argument(number=cmdIdx, value=rawChar, status=getCmdStat)
      
      ! Exit condition. Positive value means that retrieval failed.
      if (getCmdStat > 0) exit

      ! Trim extra spaces.
      cmdArg = trim(rawChar)

      ! Parse for long commands.
      if (len(cmdArg) > 2) then
        if (cmdArg(1:2) == LONG_CMD_ID) then
          call parseLongCmds(parserObj, cmdArg(3:))

          ! Get to the next command argumnet.
          cmdIdx = cmdIdx + 1
          cycle
        end if
      end if

      ! Parse for short commands.
      if (len(cmdArg) > 1) then
        if (cmdArg(1:1) == SHORT_CMD_ID) then
          call parseShortCmds(parserObj, cmdIdx, cmdArg(2:))

          ! Get to the next command argumnet.
          cmdIdx = cmdIdx + 1
          cycle
        end if
      end if

      call raiseError("Invalid command '" // cmdArg // "'.")
    end do
  end subroutine parseCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseShortCmds
  !>  Parse the command argument `cmdArg` as a short command, i.e. command
  !!  starting with a single '-' character.
  ! -------------------------------------------------------------------------- !
  subroutine parseShortCmds(parserObj, cmdIdx, cmdArg)
    type(CmdArgParser),  intent(inout) :: parserObj
      !! `CmdArgParser` object to be modified.
    integer,             intent(inout) :: cmdIdx  
      !! Index for indexing command arguments.
      !! NOTE: `cmdIdx` may mutate in this routine.
    character(len=*),    intent(in)    :: cmdArg
      !! Command argument to be parsed. This should not contain the '-'
      !! identifier at the beginning of command arguments.

    character(len=MAX_ARG_LEN) :: KVCmdValue ! Value for the key-value
    character :: currChar, cmdType
    integer :: i, getStat

    do i = 1, len(cmdArg)
      currChar = cmdArg(i:i)

      ! Get the type of the current command.
      cmdType = parserObj % cmdTypeTable % get(currChar, getStat)
      if (getStat /= HSHTBLE_STAT_OK) &
        call raiseError("Invalid command '" // SHORT_CMD_ID // currChar // "'.")

      ! Set the value for the obtained command.
      select case(cmdType)
        case(CMD_TYPE_FLAG_S)
          call parserObj % cmdValueTable % set(currChar, FLAG_TOGGLED)
          call setValToAlias(parserObj, currChar, FLAG_TOGGLED)

        case(CMD_TYPE_KEYVAL_S)
          ! Get the next command argument.
          cmdIdx = cmdIdx + 1
          call get_command_argument(number=cmdIdx, value=KVCmdValue, &
            status=getStat)

          if (getStat > 0) then
            call raiseError( &
              "No value obtained for the command '" &
              // SHORT_CMD_ID // currChar // "'." &
              )
          end if

          ! Finally set value to the key-value command.
          call parserObj % cmdValueTable % set(currChar, trim(KVCmdValue))
          call setValToAlias(parserObj, currChar, KVCmdValue)

        case(CMD_TYPE_FLAG_L)
          call raiseError("Invalid command. Did you mean '" &
            // LONG_CMD_ID // currChar // "'?"&
            )

        case(CMD_TYPE_KEYVAL_L)
          call raiseError("Invalid command. Did you mean '" &
            // LONG_CMD_ID // currChar // "=<value>'?"&
            )

        case default
          call raiseError( &
            "Internal error. Unknown type for '" &
            // SHORT_CMD_ID // currChar // "'" &
            )
      end select
    end do
  end subroutine parseShortCmds


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseLongCmds
  !>  Parse the command argument `cmdArg` as a long command, i.e. command
  !!  starting with two '-' characters.
  ! -------------------------------------------------------------------------- !
  subroutine parseLongCmds(parserObj, cmdArg)
    class(CmdArgParser), intent(inout) :: parserObj
      !! `CmdArgParser` object to be modified.
    character(len=*),    intent(in)    :: cmdArg
      !! Command argument to be parsed. This should not contain the '--'
      !! identifier at the beginning of command arguments.

    ! Key-value separator.
    character, parameter :: KEYVAL_SEP = "="

    ! Temporary character for holding either key or value substring.
    character(len=:), allocatable :: keyChar, valueChar

    character :: currChar, cmdType
    logical   :: isReadingKey
    integer   :: i, getStat

    ! Initialize local variables.
    allocate(character(len=0) :: keyChar)
    allocate(character(len=0) :: valueChar)
    isReadingKey = .true.

    ! Read command argument as if it is a key-value command; the methods
    ! for key-value and flag commands are similar.
    do i = 1, len(trim(cmdArg))
      currChar = cmdArg(i:i)

      ! Switch from reading key to reading value.
      if (currChar == KEYVAL_SEP) then
        isReadingKey = .false.
        cycle
      end if

      if (isReadingKey) then
        keyChar = keyChar // currChar
      else
        valueChar = valueChar // currChar
      end if
    end do

    ! Check if a key was indeed obtained.
    if (len(keyChar) == 0) then
      call raiseError("Missing key")
    end if

    ! Get the type of the obtained key.
    cmdType = parserObj % cmdTypeTable % get(keyChar, getStat)
    if (getStat /= HSHTBLE_STAT_OK) then
      call raiseError( &
        "Invalid command '" // LONG_CMD_ID //  keyChar // "'." &
        )
    end if

    ! Finally set the value for the obtained command.
    select case(cmdType)
      case(CMD_TYPE_FLAG_L)
        ! Check if the flag command is treated as a key-value command.
        if (len(valueChar) > 0) then
          call raiseError( &
            "The command '" // LONG_CMD_ID // keyChar // &
            "' does not accept a value." &
            )
        end if

        call parserObj % cmdValueTable % set(keyChar, FLAG_TOGGLED)
        call setValToAlias(parserObj, keyChar, FLAG_TOGGLED)

      case(CMD_TYPE_KEYVAL_L)
        if (isReadingKey .or. len(valueChar) == 0) then
          call raiseError( &
            "Missing value for the command '" // LONG_CMD_ID // keyChar // &
            "'." &
            )
        end if

        call parserObj % cmdValueTable % set(keyChar, valueChar)
        call setValToAlias(parserObj, keyChar, valueChar)

      case(CMD_TYPE_FLAG_S, CMD_TYPE_KEYVAL_S)
        call raiseError( &
          "Invalid command. Did you mean '" // SHORT_CMD_ID // keyChar // "'?" &
          )

      case default
        call raiseError( &
          "Internal error. Unknown type for '" // keyChar // "'" &
          )
    end select

    deallocate(keyChar)
    deallocate(valueChar)
  end subroutine parseLongCmds


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setValToAlias
  !>  Set the value of the alias of the command `cmdName`.
  ! -------------------------------------------------------------------------- !
  subroutine setValToAlias(parserObj, cmdName, valueChar)
    type(CmdArgParser), intent(inout) :: parserObj
      !! `CmdArgParser` object to be modified.
    character(len=*),   intent(in)    :: cmdName
      !! Command name.
    character(len=*),   intent(in)    :: valueChar
      !! Value for the command `cmdName`.

    character(len=:), allocatable :: cmdAlias
    integer :: getStat

    ! Initialize local variable.
    allocate(character(len=0) :: cmdAlias)

    cmdAlias = parserObj % cmdAliasTable % get(cmdName, getStat)
    if (getStat /= HSHTBLE_STAT_OK) return
  
    ! Set value of `cmdName` to its alias `cmdAlias`.
    call parserObj % cmdValueTable % set(cmdAlias, valueChar, getStat)
    if (getStat /= HSHTBLE_STAT_OK) then
      call raiseError("Unknown internal error encountered.")
    end if
  end subroutine setValToAlias
end submodule parserProcedures
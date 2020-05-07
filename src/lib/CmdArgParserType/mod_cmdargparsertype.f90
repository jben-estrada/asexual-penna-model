module CmdArgParserType
  ! ------------------------------------------------------------------------- !
  ! MODULE: CmdArgParserType
  ! ------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing the derived type `CmdArgParser` for parsing command
  !!  arguments.
  ! ------------------------------------------------------------------------- !
  use HashTableType, only: &
    HashTable, &
    HashTableIterator, &
    HSHTBLE_STAT_OK => STAT_OK
  use ErrorMSG, only: raiseError, raiseWarning
  implicit none
  private

  ! Command types.
  character, parameter :: CMD_TYPE_FLAG_S = "f"
    !! Flag command type (short).
  character, parameter :: CMD_TYPE_FLAG_L = "F"
    !! Flag command type (long).
  character, parameter :: CMD_TYPE_KEYVAL_S = "k"
    !! Key-value command type (short).
  character, parameter :: CMD_TYPE_KEYVAL_L = "K" 
    !! Key-value command type (long).

  character, parameter :: VOID_CHAR = achar(0)
    !! Placeholder value.

  ! Value for flags.
  character, parameter :: FLAG_TOGGLED = "T"
    !! Toggled state of flag command.

  ! Command Identifiers.
  character(len=1), parameter :: SHORT_CMD_ID = "-"
    !! Identifier for short commands.
  character(len=2), parameter :: LONG_CMD_ID = "--"
  !! Identifier for long commands.

  type :: CmdArgParser
    private
    type(HashTable) :: cmdTypeTable
      !! Table of commands and their corresponding types.
    type(HashTable) :: cmdValueTable
      !! Table of commands and their corresponding values obtained from
      !! command-line arguments.
    type(HashTable) :: cmdAliasTable
      !! Table of aliases of commands.
    type(HashTable) :: cmdUsageTable
      !! Table of usage message for commands.
    integer :: cmdCount = 0
      !! Number of defined commands defined in this `CmdArgParser` object.
  contains
    procedure :: setCmd => cmdargparser_setCmd
      !! Set a command, its type and its usage text. An alias can be optionally 
      !! be set.
    procedure :: readCmdArgs => cmdargparser_readCmdArgs
      !! Read and parse the command-line arguments.
    procedure :: hasValue => cmdargparser_hasValue
      !! Determine if a command is passed and has a value.
    procedure :: printHelp => cmdargparser_printhelp
      !! Print the help message.
    procedure :: getCmdValue => cmdargparser_getCmdValue
      !! Get the value of the given key-value command.
    procedure :: isFlagToggled => cmdargparser_isFlagToggled
      !! Determine if a flag command is passed.
  end type

  ! -------------------------------------------------------------------------- !
  ! Interface for submodule procedures.
  interface
    module subroutine parseCmdArgs(parserObj)
      class(CmdArgParser), intent(inout) :: parserObj
        !! `CmdArgParser` object.
    end subroutine parseCmdArgs

    module subroutine sortCharArr(charArr)
      character(len=*), intent(inout) :: charArr(:)
    end subroutine sortCharArr
  end interface

  ! Constructor.
  interface CmdArgParser
    module procedure :: cmdargparser_cnstrct
  end interface

  ! Public elements in this module.
  ! -------------------------------------------------------------------------- !
  public :: CmdArgParser

  public :: CMD_TYPE_FLAG_S
  public :: CMD_TYPE_FLAG_L
  public :: CMD_TYPE_KEYVAL_S
  public :: CMD_TYPE_KEYVAL_L

  public :: FLAG_TOGGLED
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: cmdargparser_cnstrct
  !>  Constructor for the `CmdArgParser` type.
  ! -------------------------------------------------------------------------- !
  function cmdargparser_cnstrct() result(new)
    type(CmdArgParser), target :: new

    ! Initialize the hash table attributes.
    new % cmdTypeTable = HashTable()
    new % cmdValueTable = HashTable()
    new % cmdAliasTable = HashTable()
    new % cmdUsageTable = HashTable()

    ! Add the command for print the help message.
    call new % setCmd("h", CMD_TYPE_FLAG_S, "Show this help message.", &
        "help", CMD_TYPE_FLAG_L)
  end function cmdargparser_cnstrct


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: cmdargparser_setCmd
  !>  Set a command, its type and its usage text. An alias can be optionally
  !!  be set.
  ! -------------------------------------------------------------------------- !
  subroutine cmdargparser_setCmd(self, cmdName, cmdType, cmdUsage, cmdAlias, &
    cmdAliasType)
    class(CmdArgParser),        intent(inout) :: self
      !! `CmdArgParser` object to be initialized.
    character(len=*),           intent(in)    :: cmdName
      !! Name of the command.
    character(len=*),           intent(in)    :: cmdType
      !! Type of the command `cmdName`.
    character(len=*),           intent(in)    :: cmdUsage
      !! Usage message for the command `cmdName`.
    character(len=*), optional, intent(in)    :: cmdAlias
      !! Name of the alias for the command `cmdName`.
    character(len=*), optional, intent(in)    :: cmdAliasType
      !! Type of the command `cmdALias`.

    character :: dummyChar 
    integer   :: getStat

    if (.not. isValidCmdType(cmdType)) then
      call raiseError("Cannot set a command. Invalid type of command.")
    end if

    ! Check if `cmdName` is already defined.
    dummyChar = self % cmdTypeTable % get(cmdName, getStat)
    if (getStat == HSHTBLE_STAT_OK) then
      call raiseWarning( &
        "Overwriting the command '(" &
        // SHORT_CMD_ID // "/" // LONG_CMD_ID // ")" // trim(cmdName) // "'" &
        )
    end if

    call self % cmdTypeTable % set(cmdName, cmdType)
    call self % cmdUsageTable % set(cmdName, cmdUsage)
    call self % cmdValueTable % set(cmdName, VOID_CHAR)

    ! Increment the number of commands.
    self % cmdCount = self % cmdCount + 1

    ! Include an alias for `cmdName`.
    if (present(cmdAlias) .or. present(cmdAliasType)) then
      if (.not. present(cmdAlias) .and. present(cmdAliasType)) then
        call raiseError( &
          "Cannot set command. " // &
          "'cmdAlias' and 'cmdAliasType' must be both present or absent." &
          )
      end if

      ! Check all invalid pair of values for `cmdAliasType` and `cmdType`.
      select case(cmdAliasType)
        case(CMD_TYPE_FLAG_L)
          if (cmdType /= CMD_TYPE_FLAG_S) then
            call raiseError( &
              "Invalid type for alias command. " // &
              "Long flag commands must only alias short flag commands." &
              )
          end if

        case(CMD_TYPE_FLAG_S)
          if (cmdType /= CMD_TYPE_FLAG_S) then
            call raiseError( &
              "Invalid type for alias command. " // &
              "Short flag commands must only alias long flag commands." &
              )
          end if

        case(CMD_TYPE_KEYVAL_L)
          if (cmdType /= CMD_TYPE_KEYVAL_S) then
            call raiseError( &
              "Invalid type for alias command. " // &
              "Long key-value commands must "   // &
              "only alias short key-value commands." &
              )
          end if

        case(CMD_TYPE_KEYVAL_S)
          if (cmdType /= CMD_TYPE_KEYVAL_L) then
            call raiseError( &
              "Invalid type for alias command. " // &
              "Short key-value commands must "   // &
              "only alias long key-value commands." &
              )
          end if

        case default
          call raiseError("Cannot set an alias for a command. Invalid type.")
      end select

      ! Check if `cmdAlias` is already defined.
      dummyChar = self % cmdTypeTable % get(cmdAlias, getStat)
      if (getStat == HSHTBLE_STAT_OK) then
        call raiseWarning("Overwriting the command '"// trim(cmdName) // "'.")
      end if

      ! Check if `cmdAlias` is already defined.
      dummyChar = self % cmdTypeTable % get(cmdAlias, getStat)
      if (getStat == HSHTBLE_STAT_OK) then
        call raiseWarning( &
          "Overwriting the command '(" &
          // SHORT_CMD_ID // "/" // LONG_CMD_ID // ")" // trim(cmdAlias) // &
          "' as alias for '" // trim(cmdName) // "'." &
          )
      end if

      ! Finally assign the alias.
      call self % cmdAliasTable % set(cmdName, cmdAlias)
      call self % cmdAliasTable % set(cmdAlias, cmdName)

      ! Assign the alias as one of the commands.
      call self % cmdTypeTable % set(cmdAlias, cmdAliasType)
      call self % cmdUsageTable % set(cmdAlias, cmdUsage)
      call self % cmdValueTable % set(cmdAlias, VOID_CHAR)

      ! Increment the number of commands.
      self % cmdCount = self % cmdCount + 1
    end if
  end subroutine cmdargparser_setCmd


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isValidCmdType
  !>  Check if `cmdType` is a valid command type.
  ! -------------------------------------------------------------------------- !
  logical pure function isValidCmdType(cmdType)
    character(len=*), intent(in) :: cmdType

    isValidCmdType = (cmdType == CMD_TYPE_FLAG_L)   .or. &
                     (cmdType == CMD_TYPE_FLAG_S)   .or. &
                     (cmdType == CMD_TYPE_KEYVAL_L) .or. &
                     (cmdType == CMD_TYPE_KEYVAL_S)
  end function isValidCmdType


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: cmdargparser_readCmdArgs
  !>  Read and parse the command-line arguments.
  ! -------------------------------------------------------------------------- !
  subroutine cmdargparser_readCmdArgs(self)
    class(CmdArgParser), intent(inout) :: self
      !! `CmdArgParser` object to be modified.

    call parseCmdArgs(self)
  end subroutine cmdargparser_readCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: cmdargparser_printhelp
  !>  Print the help message.
  ! -------------------------------------------------------------------------- !
  subroutine cmdargparser_printhelp(self, progName, progDesc)
    class(CmdArgParser), target, intent(inout) :: self
      !! `CmdArgParser` object.
    character(len=*),            intent(in)    :: progName
      !! Name of the program using this module.
    character(len=*),            intent(in)    :: progDesc
      !! Program description using this module.


    integer, parameter :: MAX_CMD_LEN = 20 ! Max length of command to display.
    integer :: i, j, getStat

    character(len=:), allocatable :: cmdNames(:)
    character(len=MAX_CMD_LEN)    :: cmdAlias
    character(len=MAX_CMD_LEN)    :: shortCmd
    character(len=MAX_CMD_LEN)    :: longCmd

    type(HashTable), pointer :: usageTable_ptr
    type(HashTableIterator)  :: usageTableIter

    ! Initialize hash table iterator.
    usageTable_ptr => self % cmdUsageTable
    usageTableIter = HashTableIterator(usageTable_ptr)

    ! Initialize array of command names.
    allocate(character(len=MAX_CMD_LEN) :: cmdNames(self % cmdCount))

    ! Get the command names.
    do i = 1, self % cmdCount
      cmdNames(i) = usageTableIter % getKey()
    end do

    ! Sort the command names.
    call sortCharArr(cmdNames)

    ! Print the header of the help message.
    print "(3a/)", "Usage: ", trim(progName), " [options]"
    print "(a/)", trim(progDesc)
    print "(a)", "Options:"

    ! Print the commands.
    do i = 1, self % cmdCount
      ! Ignore removed commands.
      if (len_trim(cmdNames(i)) == 0) cycle

      ! Identify which position in the help message does `cmdNames` occupy.
      if (isShortCmd(self, cmdNames(i))) then
        shortCmd = appendCmdID(self, cmdNames(i))
      else
        longCmd = appendCmdID(self, cmdNames(i))
      end if

      ! Check if the current command name has an alias.
      cmdAlias = self % cmdAliasTable % get(cmdNames(i), getStat)
      if (getStat == HSHTBLE_STAT_OK) then

        ! Remove the alias command from command name array.
        do j = i, self % cmdCount
          if (cmdNames(j) == cmdAlias) cmdNames(j) = ""
        end do

        ! Identify which position in the help message does `cmdNames` occupy.
        if (isShortCmd(self, cmdAlias)) then
          shortCmd = appendCmdID(self, cmdAlias)
        else
          longCmd = appendCmdID(self, cmdAlias)
        end if
      end if

      print "(' ', a10, a20, a)", &
          shortCmd, longCmd, self % cmdUsageTable % get(cmdNames(i))
    end do

    ! Free local allocated variables.
    deallocate(cmdNames)
  end subroutine cmdargparser_printhelp


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isShortCmd
  !>  Check if `cmdName` is a short command or not.
  ! -------------------------------------------------------------------------- !
  logical function isShortCmd(cmdParser, cmdName)
    class(CmdArgParser), intent(inout) :: cmdParser
    character(len=*),    intent(in)    :: cmdName

    character :: cmdType

    cmdType = cmdParser % cmdTypeTable % get(cmdName)

    isShortCmd = (cmdType == CMD_TYPE_FLAG_S .or.cmdType == CMD_TYPE_KEYVAL_S)
  end function isShortCmd


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: appendCmdID
  !>  Append characters around command name `cmdName` depending on its type.
  ! -------------------------------------------------------------------------- !
  function appendCmdID(cmdParser, cmdName) result(cmd)
    class(CmdArgParser), intent(inout) :: cmdParser
    character(len=*),    intent(in)    :: cmdName

    character(len=:),    allocatable   :: cmd

    allocate(character(len=0) :: cmd)

    select case(cmdParser % cmdTypeTable % get(cmdName))
      case(CMD_TYPE_KEYVAL_L)
        cmd = LONG_CMD_ID // trim(cmdName) // "=<val>"
      case(CMD_TYPE_KEYVAL_S)
        cmd = SHORT_CMD_ID // trim(cmdName) // " <val>"
      case(CMD_TYPE_FLAG_L)
        cmd = LONG_CMD_ID // trim(cmdName)
      case(CMD_TYPE_FLAG_S)
        cmd = SHORT_CMD_ID // trim(cmdName)
      case default
        call raiseError("Unknown internal error encountered.")
    end select
  end function appendCmdID


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: cmdargparser_getCmdValue
  !>  Get the value of the key-value command `cmdName`.
  ! -------------------------------------------------------------------------- !
  function cmdargparser_getCmdValue(self, cmdName) result(cmdValue)
    class(CmdArgParser), intent(inout) :: self
      !! `CmdArgParser` object to be searched.
    character(len=*),    intent(in)    :: cmdName
      !! Name of the command whose value is to be searched.

    character(len=:), allocatable :: cmdValue
      !! Output value.

    character :: cmdType
    integer   :: getStat

    ! Initialize output.
    allocate(character(len=0) :: cmdValue)

    ! Get the type of `cmdName` to check its type and its existence as well.
    cmdType = self % cmdTypeTable % get(cmdName, getStat)

    ! Check the type of the command.
    if (cmdType /= CMD_TYPE_KEYVAL_L .and. cmdType /= CMD_TYPE_KEYVAL_S) then
      call raiseError( &
        "'(" // SHORT_CMD_ID // LONG_CMD_ID // ")" // trim(cmdName) // &
        "' is not a key-value command." &
        )
    end if

    if (getStat /= HSHTBLE_STAT_OK) then
      call raiseError("Unknown command name '" // trim(cmdName) // "'.")
    end if

    ! Get the value.
    cmdValue = self % cmdValueTable % get(cmdName)
    
    ! Check if a value is obtained for the command `cmdName`.
    if (cmdValue == VOID_CHAR) then
  
      call raiseError( &
        "No value obtained for the command '(" &
        // SHORT_CMD_ID // "/" // LONG_CMD_ID // ")" &
        // trim(cmdName) // "'."&
        )
    end if
  end function cmdargparser_getCmdValue


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: cmdargparser_hasValue
  !>  Check if `cmdName` has a value or not.
  ! -------------------------------------------------------------------------- !
  logical function cmdargparser_hasValue(self, cmdName)
    class(CmdArgParser), intent(inout) :: self
      !! `CmdArgParser` object to be searched for
    character(len=*),    intent(in)    :: cmdName
      !! Name of the command to be inspected.

    character :: dummyChar
    integer   :: getStat

    dummyChar = self % cmdValueTable % get(cmdName, getStat)
    if (getStat /= HSHTBLE_STAT_OK) then
      call raiseError("Unknown command name '" // trim(cmdName) // "'.")
    end if

    cmdargparser_hasValue = (dummyChar /= VOID_CHAR)
  end function cmdargparser_hasValue


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: cmdargparser_isFlagToggled
  !>  Check if the flag command `cmdName` is toggled or not.
  ! -------------------------------------------------------------------------- !
  logical function cmdargparser_isFlagToggled(self, cmdName)
    class(CmdArgParser), intent(inout) :: self
      !! `CmdArgParser` object to be searched for
    character(len=*),    intent(in)    :: cmdName
      !! Name of the command to be inspected.
    
    character :: cmdType
    integer   :: getStat

    cmdType = self % cmdTypeTable % get(cmdName, getStat)
    if (getStat /= HSHTBLE_STAT_OK) then
      call raiseError("Unknown command name '" // trim(cmdName) // "'.")
    end if

    ! Check type of the command.
    if (cmdType /= CMD_TYPE_FLAG_S .and. cmdType /= CMD_TYPE_FLAG_L) then
      call raiseError( &
        "'(" // SHORT_CMD_ID // "/" //LONG_CMD_ID // ")" // trim(cmdName) // &
        "' is not a flag command." &
        )
    end if

    cmdargparser_isFlagToggled = &
      (self % cmdValueTable % get(cmdName) == FLAG_TOGGLED)
  end function cmdargparser_isFlagToggled
end module CmdArgParserType

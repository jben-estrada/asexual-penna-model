module CmdArgParserType
  ! ------------------------------------------------------------------------- !
  ! MODULE: CmdArgParserType
  ! ------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing the derived type `CmdArgParser_t` for parsing command
  !!  arguments.
  ! ------------------------------------------------------------------------- !
  use HashTableType, only:  &
    HashTable_t,            &
    HashTableIterator_t,    &
    init_HashTable,         &
    init_HashTableIterator, &
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

  type :: CmdArgParser_t
    private
    type(HashTable_t) :: cmdTypeTable
      !! Table of commands and their corresponding types.
    type(HashTable_t) :: cmdValueTable
      !! Table of commands and their corresponding values obtained from
      !! command-line arguments.
    type(HashTable_t) :: cmdAliasTable
      !! Table of aliases of commands.
    type(HashTable_t) :: cmdUsageTable
      !! Table of usage message for commands.
    type(HashTable_t) :: cmdGroupTable
      !! Table of optional command groupings.
    integer :: cmdCount = 0
      !! Number of defined commands defined in this `CmdArgParser_t` object.
    integer :: cmdGroupCount = 1
      !! Number of command groupings. All commands belong to a group.
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
  end type CmdArgParser_t

  ! -------------------------------------------------------------------------- !
  ! Interface for submodule procedures.
  interface
    module subroutine parseCmdArgs(parserObj)
      class(CmdArgParser_t), intent(inout) :: parserObj
        !! `CmdArgParser_t` object.
    end subroutine parseCmdArgs

    module subroutine sortCharArr(charArr)
      character(len=*), intent(inout) :: charArr(:)
    end subroutine sortCharArr
  end interface

  ! Public elements in this module.
  ! -------------------------------------------------------------------------- !
  public :: CmdArgParser_t
  public :: init_CmdArgParser

  public :: CMD_TYPE_FLAG_S
  public :: CMD_TYPE_FLAG_L
  public :: CMD_TYPE_KEYVAL_S
  public :: CMD_TYPE_KEYVAL_L

  public :: FLAG_TOGGLED
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: init_CmdArgParser
  !>  Initializer of `CmdArgParser_t` objects.
  ! -------------------------------------------------------------------------- !
  subroutine init_CmdArgParser(new)
    type(CmdArgParser_t), target, intent(inout) :: new

    ! Initialize the hash table attributes.
    call init_HashTable(new % cmdTypeTable)
    call init_HashTable(new % cmdValueTable)
    call init_HashTable(new % cmdAliasTable)
    call init_HashTable(new % cmdUsageTable)
    call init_HashTable(new % cmdGroupTable)

    ! Add the command for print the help message.
    call new % setCmd("h", CMD_TYPE_FLAG_S, "Show this help message.", &
        "help", CMD_TYPE_FLAG_L)
    end subroutine init_CmdArgParser


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: cmdargparser_setCmd
  !>  Set a command, its type and its usage text. An alias can be optionally
  !!  be set.
  ! -------------------------------------------------------------------------- !
  subroutine cmdargparser_setCmd(   &
        self, cmdName, cmdType, cmdUsage, cmdAlias, cmdAliasType, cmdGroup  &
      )
    class(CmdArgParser_t),      intent(inout) :: self
      !! `CmdArgParser_t` object to be initialized.
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
    character(len=*), optional, intent(in)    :: cmdGroup
      !! Name of the group the command is in. Defaults to None.

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

    call self % cmdTypeTable  % set(cmdName, cmdType)
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
      call self % cmdTypeTable  % set(cmdAlias, cmdAliasType)
      call self % cmdUsageTable % set(cmdAlias, cmdUsage)
      call self % cmdValueTable % set(cmdAlias, VOID_CHAR)

      ! Increment the number of commands.
      self % cmdCount = self % cmdCount + 1
    end if

    ! Set the group for the given command.
    if (present(cmdGroup)) then
      call self % cmdGroupTable % set(cmdName, cmdGroup)
      self % cmdGroupCount = self % cmdGroupCount + 1
    else
      call self % cmdGroupTable % set(cmdName, VOID_CHAR)
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
    class(CmdArgParser_t), intent(inout) :: self
      !! `CmdArgParser_t` object to be modified.
    call parseCmdArgs(self)
  end subroutine cmdargparser_readCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: cmdargparser_printhelp
  !>  Print the help message.
  ! -------------------------------------------------------------------------- !
  subroutine cmdargparser_printhelp(self, progName, progDesc)
    class(CmdArgParser_t), target, intent(inout) :: self
      !! `CmdArgParser_t` object.
    character(len=*),              intent(in)    :: progName
      !! Name of the program using this module.
    character(len=*),              intent(in)    :: progDesc
      !! Program description using this module.

    integer, parameter :: MAX_CMD_LEN = 25 ! Max length of command to display.
    integer :: cmdIdx, groupIdx, k
    integer :: cmdGroupCount, uniqueCmdCount
    integer :: getStat
    logical :: groupExists, cmdIsShort, aliasIsShort

    character(len=MAX_CMD_LEN), allocatable :: cmdNames(:)
    character(len=MAX_CMD_LEN), allocatable :: cmdGroups(:)
    character(len=MAX_CMD_LEN), allocatable :: cmdNameGroupMap(:, :)
    character(len=MAX_CMD_LEN) :: cmdName
    character(len=MAX_CMD_LEN) :: cmdAlias
    character(len=MAX_CMD_LEN) :: shortCmd
    character(len=MAX_CMD_LEN) :: longCmd
    character(len=MAX_CMD_LEN) :: cmdGroupName

    character(len=:), allocatable :: groupHeader

    ! NOTE: We use the cmdGroupTable since it contains all commands excluding
    !       their respective aliases if they exist.
    type(HashTable_t), pointer :: groupTable_ptr
    type(HashTableIterator_t)  :: groupTableIter
    
    ! Initialize the hash table iterator
    groupTable_ptr => self % cmdGroupTable
    call init_HashTableIterator(groupTableIter, groupTable_ptr)

    allocate(cmdNames(self % cmdCount))
    allocate(cmdGroups(self % cmdGroupCount))
    allocate(cmdNameGroupMap(self % cmdGroupCount, 0:self % cmdCount))

    cmdGroupCount  = 0
    uniqueCmdCount = 0
    ! Get all the command names
    do
      cmdName = groupTableIter % getKey(getStat)
      if (getStat /= 0) exit   ! End of the hash table iterator.

      uniqueCmdCount = uniqueCmdCount + 1
      cmdNames(uniqueCmdCount) = cmdName
      cmdGroupName = self % cmdGroupTable % get(cmdName)

      ! Check if the current command's group has already been registered in the
      ! group array.
      groupExists = .false.
      do groupIdx = 1, cmdGroupCount
        if (cmdGroups(groupIdx) == trim(cmdGroupName)) then
          groupExists = .true.
          exit
        end if
      end do

      if (.not.groupExists) then
        cmdGroupCount = cmdGroupCount + 1
        cmdGroups(cmdGroupCount) = cmdGroupName
      end if
    end do

    call sortCharArr(cmdGroups)
    call sortCharArr(cmdNames)

    ! Set the header of the command name-group mapping array.
    cmdNameGroupMap(:, :) = " "
    cmdNameGroupMap(:, 0) = cmdGroups(:)

    ! Group the command names by their respective groups.
    groupingCmd: do cmdIdx = 1, uniqueCmdCount
      cmdName = cmdNames(cmdIdx)
      cmdGroupName = self % cmdGroupTable % get(cmdName)

      ! Find the current command's group in the cmdname-group mapping array.
      groupExists = .false.
      findGroup: do groupIdx = 1, cmdGroupCount
        if (cmdGroupName == cmdNameGroupMap(groupIdx, 0)) then
          groupExists = .true.

          appendCmdToGroup: do k = 1, uniqueCmdCount
            if (len_trim(cmdNameGroupMap(groupIdx, k)) == 0) then
              cmdNameGroupMap(groupIdx, k) = cmdName
              exit appendCmdToGroup
            end if
          end do appendCmdToGroup

          exit
        end if
      end do findGroup

      if (.not.groupExists) then
        call raiseError(  &
            "Cannot find the group for the command '" // trim(cmdName) // "'"  &
        )
      end if
    end do groupingCmd

    ! Print the header of the help message.
    print "(3a/)", "Usage: ", trim(progName), " [options...]"
    print "(a/)", trim(progDesc)
    print "((40(' '), a))", "PROGRAM OPTIONS"
    print "((40(' '), a))", repeat("-", 15)

    do groupIdx = 1, cmdGroupCount
      cmdGroupName = cmdNameGroupMap(groupIdx, 0)

      ! Print the group option header.
      if (cmdGroupName == VOID_CHAR) then
        groupHeader = "General options"
      else
        groupHeader = trim(cmdGroupName) // " options"
      end if
      print "(a, /a)", groupHeader, repeat("-", len(groupHeader)) 

      printCmd: do cmdIdx = 1, uniqueCmdCount
        cmdName = cmdNameGroupMap(groupIdx, cmdIdx)

        if (len_trim(cmdName) == 0) exit printCmd

        ! Identify which position in the help message `cmdNames` occupies.
        cmdIsShort = isShortCmd(self, cmdName)
        if (cmdIsShort) then
          shortCmd = appendCmdID(self, cmdName)
        else
          longCmd = appendCmdID(self, cmdName)
        end if

        ! Check if the current command has an alias.
        cmdAlias = self % cmdAliasTable % get(cmdName, getStat)
        if (getStat == HSHTBLE_STAT_OK) then
          ! Identify which position in the help message does `cmdNames` occupy.
          aliasIsShort = isShortCmd(self, cmdAlias)
          ! NOTE: If the command and its alias are of the same type,
          !       the original command has higher precendence.
          if (aliasIsShort .neqv. cmdIsShort) then
            if (aliasIsShort) then
              shortCmd = appendCmdID(self, cmdAlias)
            else
              longCmd = appendCmdID(self, cmdAlias)
            end if
          end if
        end if

        print "(' ', a10, a25, a)", &
          shortCmd, longCmd, self % cmdUsageTable % get(cmdName)
      end do printCmd
    end do

    if (allocated(groupHeader)) deallocate(groupHeader)
    deallocate(cmdNames, cmdGroups, cmdNameGroupMap)
  end subroutine cmdargparser_printhelp


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isShortCmd
  !>  Check if `cmdName` is a short command or not.
  ! -------------------------------------------------------------------------- !
  logical function isShortCmd(cmdParser, cmdName)
    class(CmdArgParser_t), intent(inout) :: cmdParser
    character(len=*),      intent(in)    :: cmdName

    character :: cmdType

    cmdType = cmdParser % cmdTypeTable % get(cmdName)

    isShortCmd = (cmdType == CMD_TYPE_FLAG_S .or.cmdType == CMD_TYPE_KEYVAL_S)
  end function isShortCmd


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: appendCmdID
  !>  Append characters around command name `cmdName` depending on its type.
  ! -------------------------------------------------------------------------- !
  function appendCmdID(cmdParser, cmdName) result(cmd)
    class(CmdArgParser_t), intent(inout) :: cmdParser
    character(len=*),      intent(in)    :: cmdName

    character(len=:),    allocatable   :: cmd

    allocate(character(len=0) :: cmd)

    select case(cmdParser % cmdTypeTable % get(cmdName))
      case(CMD_TYPE_KEYVAL_L)
        cmd = LONG_CMD_ID // trim(cmdName) // "=<VAL>"
      case(CMD_TYPE_KEYVAL_S)
        cmd = SHORT_CMD_ID // trim(cmdName) // " <VAL>"
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
    class(CmdArgParser_t), intent(inout) :: self
      !! `CmdArgParser_t` object to be searched.
    character(len=*),      intent(in)    :: cmdName
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
    class(CmdArgParser_t), intent(inout) :: self
      !! `CmdArgParser_t` object to be searched for
    character(len=*),      intent(in)    :: cmdName
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
    class(CmdArgParser_t), intent(inout) :: self
      !! `CmdArgParser_t` object to be searched for
    character(len=*),      intent(in)    :: cmdName
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

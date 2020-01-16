module CmdOptionType
  implicit none
  private

  ! Buffer string length.
  integer, public, parameter :: MAX_LEN = 64
  integer, public, parameter :: LONG_MAX_LEN = 256

  ! Key-value separator.
  character, parameter :: KEY_VAL_SEP = "="

  ! Null character.
  character, public, parameter :: NULL_CHAR = ""

  ! -------------------------------------------------------------------------- !
  ! DERIVED TYPE DEFINITIONS.
  ! -------------------------------------------------------------------------- !
  ! Base type for all command-line argument type.
  type, public, abstract :: BaseCmdOption
    private
    character(len=MAX_LEN)      :: command
    character(len=MAX_LEN)      :: altCommand
    character(len=LONG_MAX_LEN) :: usageMsg

    logical :: isOptional = .false.
    logical :: hasValue = .false.
  contains
    procedure :: getCommand
    procedure :: getAltCommand
    procedure :: getUsageMsg
  end type BaseCmdOption
  ! -------------------------------------------------------------------------- !

  ! Key-value command-line argument type.
  type, public, extends(BaseCmdOption) :: KeyValCmdOption
    private
    integer :: value
  contains
    procedure :: getValue => KVtype_getValue
    procedure, private :: KVtype_getValue
  end type KeyValCmdOption
  ! -------------------------------------------------------------------------- !

  ! Flag command-line argument type.
  type, public, extends(BaseCmdOption) :: FlagCmdOption
    private
    logical :: state = .false.
  contains
    procedure :: getFlagState => flagtype_getFlagState
    procedure, private :: flagtype_getFlagState
  end type FlagCmdOption
  ! -------------------------------------------------------------------------- !

  ! Positional argument type.
  type, public, extends(BaseCmdOption) :: PositionalCmdOption
    private
    character(len=LONG_MAX_LEN) :: value
    integer :: position = -1
  contains
    procedure :: getValue => posType_getValue
    procedure :: getPosition => posType_getPosition

    procedure, private :: posType_getValue
    procedure, private :: posType_getPosition
  end type PositionalCmdOption
  ! -------------------------------------------------------------------------- !

  ! -------------------------------------------------------------------------- !
  ! SUBMODULE PROCEDURE INTERFACES
  ! -------------------------------------------------------------------------- !
  ! 'BaseCmdOption' type-bound procedure.
  interface
    module subroutine setUsageMsg(cmdOption, usageMsg)
      class(BaseCmdOption), intent(inout) :: cmdOption
      character(len=*),     intent(in)    :: usageMsg
    end subroutine

    module pure function getCommand(self)
      class(BaseCmdOption), intent(in) :: self
      character(len=LONG_MAX_LEN) :: getCommand
    end function

    module pure function getAltCommand(self)
      class(BaseCmdOption), intent(in) :: self
      character(len=MAX_LEN) :: getAltCommand
    end function

    module pure function getUsageMsg(self)
      class(BaseCmdOption), intent(in) :: self
      character(len=LONG_MAX_LEN) :: getUsageMsg
    end function
  end interface
  ! -------------------------------------------------------------------------- !

  ! 'FlagCmdOption' type-bound procedure.
  interface
    module pure function flagtype_getFlagState(self)
      class(FlagCmdOption), intent(in) :: self
      logical :: flagtype_getFlagState
    end function

    module subroutine assignInitialFlagState(cmdFlag, state)
      class(FlagCmdOption), intent(inout) :: cmdFlag
      logical,              intent(in)    :: state
    end subroutine  
  end interface
  ! -------------------------------------------------------------------------- !

  ! 'KeyValCmdOption' type-bound procedure.
  interface
    module pure function KVtype_getValue(self)
      class(KeyValCmdOption), intent(in) :: self
      integer :: KVtype_getValue
    end function

    module subroutine assignOptionalKVVal(cmdKeyVal, value)
      class(KeyValCmdOption), intent(inout) :: cmdKeyVal
      integer,                intent(in)    :: value
    end subroutine
  end interface
  ! -------------------------------------------------------------------------- !

  ! 'PositionalCmdOption' type-bound procedures.
  interface
    module subroutine setPosTypePosition(cmdPosOption, position)
      class(PositionalCmdOption), intent(inout) :: cmdPosOption
      integer,                    intent(in)    :: position
    end subroutine

    module pure function posType_getValue(self)
      class(PositionalCmdOption), intent(in) :: self
      character(len=LONG_MAX_LEN) :: posType_getValue
    end function

    module pure function posType_getPosition(self)
      class(PositionalCmdOption), intent(in) :: self
      integer :: posType_getPosition
    end function

    module subroutine assignOptionalPosTypeVal(cmdPosArg, value)
      class(PositionalCmdOption),  intent(inout) :: cmdPosArg
      character(len=LONG_MAX_LEN), intent(in)    :: value
    end subroutine
  end interface
  ! -------------------------------------------------------------------------- !

  ! Interface procedures.
  interface
    module subroutine initializeCmdOption(cmdOption, command, altCommand)
      class(BaseCmdOption),       intent(out) :: cmdOption
      character(len=*),           intent(in)  :: command
      character(len=*), optional, intent(in)  :: altCommand
    end subroutine

    module subroutine parseCmdArgs(cmdFlags, cmdKeyVal, cmdPosArgs)
      class(FlagCmdOption),       intent(inout) :: cmdFlags(:)
      class(KeyValCmdOption),     intent(inout) :: cmdKeyVal(:)
      class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
    end subroutine
  end interface
  ! -------------------------------------------------------------------------- !

  public :: parseCmdArgs
  public :: initializeCmdOption
  public :: setUsageMsg
  public :: setPosTypePosition

  public :: assignOptionalPosTypeVal
  public :: assignOptionalKVVal
  public :: assignInitialFlagState
end module CmdOptionType

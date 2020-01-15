

module CmdOptionType
  implicit none
  private

  ! Buffer string length.
  integer, public, parameter :: MAX_LEN = 64

  ! Key-value separator.
  character, parameter :: KEY_VAL_SEP = "="

  ! Null character.
  character, public, parameter :: NULL_CHAR = ""
  
  ! DERIVED TYPE DEFINITIONS.
  ! -------------------------------------------------------------------------- !
  ! Base type for all command-line argument type.
  type, public, abstract :: BaseCmdOption
    private
    character(len=:), allocatable :: command
    character(len=:), allocatable :: altCommand
    character(len=:), allocatable :: usageMsg
    logical :: isOptional = .false.
    logical :: isInitialized = .false.
  contains
    procedure :: setCommand
    procedure :: getCommand
    procedure :: getAltCommand
    procedure :: getUsageMsg
    procedure :: setUsageMsg
  end type BaseCmdOption

  ! Key-value command-line argument type.
  type, public, extends(BaseCmdOption) :: KeyValCmdOption
    private
    integer, allocatable :: value
  contains
    procedure :: getValue => KVtype_getValue
    procedure :: setValue => KVtype_setValue
    procedure :: allocatedValue => KVtype_allocatedValue
    final     :: finalizeKVObj

    procedure, private :: KVtype_allocatedValue
    procedure, private :: KVtype_getValue
    procedure, private :: KVtype_setValue
  end type KeyValCmdOption

  ! Flag command-line argument type.
  type, public, extends(BaseCmdOption) :: FlagCmdOption
    private
    logical :: state = .false.
  contains
    procedure :: toggle => flagtype_toggle
    procedure :: getFlagState => flagtype_getFlagState
    final     :: finalizeFlagObj

    procedure, private :: flagtype_toggle
    procedure, private :: flagtype_getFlagState
  end type FlagCmdOption

  ! Positional argument type.
  type, public, extends(BaseCmdOption) :: PositionalCmdOption
    private
    character(len=:), allocatable :: value
    integer :: position = -1
  contains
    procedure :: getValue => posType_getValue
    procedure :: setValue => posType_setValue
    procedure :: getPosition => posType_getPosition
    procedure :: setPosition => posType_setPosition
    procedure :: allocatedValue => posType_allocatedValue
    final     :: finalizePosObj

    procedure, private :: posType_allocatedValue
    procedure, private :: posType_getValue
    procedure, private :: posType_setValue
    procedure, private :: posType_setPosition
    procedure, private :: posType_getPosition
  end type PositionalCmdOption

  ! SUBMODULE PROCEDURE INTERFACES
  ! -------------------------------------------------------------------------- !
  ! 'BaseCmdOption' type-bound procedure.
  interface
    module subroutine setCommand(self, command, altCommand)
      class(BaseCmdOption),       intent(inout) :: self
      character(len=*),           intent(in)  :: command
      character(len=*), optional, intent(in)  :: altCommand
    end subroutine

    module pure function getCommand(self)
      class(BaseCmdOption), intent(in) :: self
      character(len=:), allocatable    :: getCommand
    end function

    module pure function getAltCommand(self)
      class(BaseCmdOption), intent(in) :: self
      character(len=:), allocatable    :: getAltCommand
    end function

    module subroutine setUsageMsg(self, usageMsg)
      class(BaseCmdOption), intent(inout) :: self
      character(len=*),     intent(in)    :: usageMsg
    end subroutine

    module pure function getUsageMsg(self)
      class(BaseCmdOption), intent(in) :: self
      character(len=:), allocatable    :: getUsageMsg
    end function
  end interface

  ! 'FlagCmdOption' type-bound procedure.
  interface
    module logical function flagtype_getFlagState(self)
      class(FlagCmdOption), intent(in) :: self
    end function

    module subroutine flagtype_toggle(self)
      class(FlagCmdOption), intent(inout) :: self
    end subroutine

    module subroutine finalizeFlagObj(self)
      type(FlagCmdOption), intent(inout) :: self
    end subroutine
  end interface

  ! 'KeyValCmdOption' type-bound procedure.
  interface
    module logical function KVtype_allocatedValue(self)
      class(KeyValCmdOption), intent(in) :: self
    end function

    module integer function KVtype_getValue(self)
      class(KeyValCmdOption), intent(in) :: self
    end function

    module subroutine KVtype_setValue(self, value)
      class(KeyValCmdOption), intent(inout) :: self
      integer,                intent(in)    :: value
    end subroutine

    module subroutine finalizeKVObj(self)
      type(KeyValCmdOption), intent(inout) :: self
    end subroutine
  end interface


  ! 'PositionalCmdOption' type-bound procedures.
  interface
    module logical function posType_allocatedValue(self)
      implicit none
      class(PositionalCmdOption), intent(in) :: self
    end function

    module function posType_getValue(self)
      class(PositionalCmdOption), intent(in) :: self
      character(len=:), allocatable          :: posType_getValue
    end function

    module subroutine posType_setValue(self, value)
      class(PositionalCmdOption), intent(inout) :: self
      character(len=*),           intent(in)    :: value
    end subroutine

    module subroutine posType_setPosition(self, position)
      class(PositionalCmdOption), intent(inout) :: self
      integer,                    intent(in)    :: position
    end subroutine

    module integer function posType_getPosition(self)
      class(PositionalCmdOption), intent(inout) :: self
    end function

    module subroutine finalizePosObj(self)
      type(PositionalCmdOption), intent(inout) :: self
    end subroutine
  end interface

  interface
    module subroutine parseCmdArgs(cmdFlags, cmdKeyVal, cmdPosArgs)
      class(FlagCmdOption),       intent(inout) :: cmdFlags(:)
      class(KeyValCmdOption),     intent(inout) :: cmdKeyVal(:)
      class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
    end subroutine
  end interface
  ! -------------------------------------------------------------------------- !

  public :: parseCmdArgs
end module CmdOptionType

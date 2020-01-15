

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
  ! -------------------------------------------------------------------------- !

  public :: getKeyVal
contains


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
end module CmdOptionType



submodule (CmdOptionType) baseOptionProcedure
  implicit none
contains


  subroutine setCommand(self, command, altCommand)
    implicit none
    class(BaseCmdOption),       intent(inout) :: self
    character(len=*),           intent(in)    :: command
    character(len=*), optional, intent(in)    :: altCommand

    ! Assign values of members. NOTE: Automatic allocation.
    ! NOTE: For some reason, this causes a memory leak. I cannot fix it.
    self % command = command

    ! Assign alternative command. NOTE: Automatic allocation.
    ! NOTE: For some reason, this causes a memory leak. I cannot fix it.
    if (present(altCommand)) self % altCommand = altCommand
  end subroutine setCommand


  pure function getCommand(self) result(command)
    implicit none
    class(BaseCmdOption), intent(in) :: self

    character(len=:), allocatable :: command

    if (allocated(self % command)) then
      command = self % command
    else
      command = NULL_CHAR
    end if
  end function getCommand


  pure function getAltCommand(self) result(altCommand)
    implicit none
    class(BaseCmdOption), intent(in) :: self

    character(len=:), allocatable :: altCommand

    if (allocated(self % altCommand)) then
      altCommand = self % altCommand
    else
      altCommand = NULL_CHAR
    end if
  end function getAltCommand


  subroutine setUsageMsg(self, usageMsg)
    implicit none
    class(BaseCmdOption), intent(inout) :: self
    character(len=*),     intent(in)    :: usageMsg

    ! NOTE: Automatic allocation.
    ! NOTE: For some reason, this causes a memory leak. I cannot fix it.
    self % usageMsg = usageMsg
  end subroutine setUsageMsg


  pure function getUsageMsg(self) result(usageMsg)
    implicit none
    class(BaseCmdOption), intent(in) :: self

    character(len=:), allocatable :: usageMsg

    if (allocated(self % usageMsg)) then
      usageMsg = self % usageMsg
    else
      usageMsg = NULL_CHAR
    end if
  end function getUsageMsg
end submodule baseOptionProcedure



submodule (CmdOptionType) flagBoundProcedure
  implicit none
contains


  logical function flagtype_getFlagState(self)
    implicit none
    class(FlagCmdOption), intent(in) :: self

    flagtype_getFlagState = self % state
  end function flagtype_getFlagState


  subroutine flagtype_toggle(self)
    implicit none
    class(FlagCmdOption), intent(inout) :: self

    self % state = .not. self % state
  end subroutine flagtype_toggle


  subroutine finalizeFlagObj(self)
    implicit none
    type(FlagCmdOption), intent(inout) :: self

    if (allocated(self % command)) deallocate(self % command)
    if (allocated(self % altCommand)) deallocate(self % altCommand)
  end subroutine finalizeFlagObj
end submodule flagBoundProcedure



submodule (CmdOptionType) KVBoundProcedure
  implicit none
contains


  logical function KVtype_allocatedValue(self)
    implicit none
    class(KeyValCmdOption), intent(in) :: self
    
    KVtype_allocatedValue = allocated(self % value)
  end function KVtype_allocatedValue


  integer function KVtype_getValue(self)
    implicit none
    class(KeyValCmdOption), intent(in) :: self

    if (allocated(self % value)) then
      KVtype_getValue = self % value
    else
      print "(3a)", "***ERROR. '", self % getCommand(), "' has no value."
      stop
    end if
  end function KVtype_getValue


  subroutine KVtype_setValue(self, value)
    implicit none
    class(KeyValCmdOption), intent(inout) :: self
    integer,                intent(in)    :: value

    ! NOTE: Automatic allocation.
    self % value = value
  end subroutine KVtype_setValue


  subroutine finalizeKVObj(self)
    implicit none
    type(KeyValCmdOption), intent(inout) :: self

    if (allocated(self % command)) deallocate(self % command)
    if (allocated(self % altCommand)) deallocate(self % altCommand)
    if (allocated(self % value)) deallocate(self % value)
  end subroutine finalizeKVObj
end submodule KVBoundProcedure


submodule (CmdOptionType) posBoundProcedure
  implicit none
contains


  logical function posType_allocatedValue(self)
    implicit none
    class(PositionalCmdOption), intent(in) :: self
    
    posType_allocatedValue = allocated(self % value)
  end function posType_allocatedValue


  function posType_getValue(self) result(value)
    implicit none
    class(PositionalCmdOption), intent(in) :: self

    character(len=:), allocatable :: value
    
    if (allocated(self % value)) then
      value = self % value
    else
      print "(3a)", "***ERROR. '", self % getCommand(), "' has no value."
      stop
    end if
  end function posType_getValue


  subroutine posType_setValue(self, value)
    implicit none

    class(PositionalCmdOption), intent(inout) :: self
    character(len=*),           intent(in)    :: value

    ! NOTE: Automatic allocation.
    self % value = value
  end subroutine posType_setValue


  subroutine posType_setPosition(self, position)
    implicit none

    class(PositionalCmdOption), intent(inout) :: self
    integer,                    intent(in)    :: position
    
    self % position = position 
  end subroutine posType_setPosition


  integer function posType_getPosition(self)
    implicit none
    class(PositionalCmdOption), intent(inout) :: self

    posType_getPosition = self % position
  end function posType_getPosition


  subroutine finalizePosObj(self)
    implicit none
    type(PositionalCmdOption), intent(inout) :: self
  
    if (allocated(self % command)) deallocate(self % command)
    if (allocated(self % altCommand)) deallocate(self % altCommand)
    if (allocated(self % value)) deallocate(self % value)
  end subroutine finalizePosObj
end submodule posBoundProcedure

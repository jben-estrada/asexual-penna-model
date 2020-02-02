module CmdOptionType
  ! -------------------------------------------------------------------------- !
  ! MODULE: CmdOptionType
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing derived type definitions of `BaseCmdOption` and its
  !!  extensions.
  ! -------------------------------------------------------------------------- !
  implicit none
  private

  ! Buffer string length.
  integer, public, parameter :: MAX_LEN = 64
  integer, public, parameter :: LONG_MAX_LEN = 256

  ! Key-value separator.
  character, parameter :: KEY_VAL_SEP = "="

  ! Null character.
  character, public, parameter :: NULL_CHAR = ""

  ! States.
  character, parameter :: TRUE_FLAG = "T"
  character, parameter :: FALSE_FLAG = "F"


  ! DERIVED TYPE DEFINITIONS.
  ! -------------------------------------------------------------------------- !
  type, public, abstract :: BaseCmdOption
    !! Base type for all command-line argument type.
    private
    character(len=MAX_LEN)      :: command
      !! Character which this command-line option is invoked with. 
    character(len=MAX_LEN)      :: altCommand
      !! Alternative to the `command` attribute.
    character(len=LONG_MAX_LEN) :: usageMsg
      !! The help message for this command-line option.
    character(len=LONG_MAX_LEN) :: value = ""

    logical :: isOptional = .false.
      !! Mark whether this command-line option is optional or not.
      !! If this is true, the option's `value` or `state` attribute must be
      !! initialized with a default value. Defaults to `.false.`.
    logical :: hasValue = .false.
      !! Mark whether this option has assigned value or not. Defaults to
      !! `.false.`.
  contains
    procedure :: getCommand
      !! Get the `command` character.
    procedure :: getAltCommand
      !! Get the alternative character to `command`, `altCommand`.
    procedure :: getUsageMsg
      !! Get the usage message of the this command-line option.
    procedure :: getValue
  end type BaseCmdOption
  ! -------------------------------------------------------------------------- !

  type, public, extends(BaseCmdOption) :: KeyValCmdOption
    !! Key-value command-line option type.
    private
    character(len=MAX_LEN) :: valueMsg
      !! Description of the value of this command-line option.
  end type KeyValCmdOption
  ! -------------------------------------------------------------------------- !

  type, public, extends(BaseCmdOption) :: FlagCmdOption
    !! Flag command-line option.
    private  
    logical :: isToggled = .false.
  contains
    procedure :: getFlagState => flagtype_getFlagState
      !! Get the state of this command-line flag.
  end type FlagCmdOption
  ! -------------------------------------------------------------------------- !

  type, public, extends(BaseCmdOption) :: PositionalCmdOption
    ! Positional command-line option type.
    private
    integer :: position = -1
      !! The position of this command-line option.
  contains
    procedure :: getPosition => posType_getPosition
      !! Get the position of this positional command-line option.
      !! Alias for `posType_getPosition`.
    procedure, private :: posType_getPosition
  end type PositionalCmdOption
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

    module pure function getValue(self)
      class(BaseCmdOption), intent(in) :: self
      character(len=LONG_MAX_LEN)      :: getValue
    end function
  end interface

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

  ! 'KeyValCmdOption' type-bound procedure.
  interface
    module subroutine assignOptionalKVVal(cmdKeyVal, value)
      class(KeyValCmdOption), intent(inout) :: cmdKeyVal
      character(len=*),       intent(in)    :: value
    end subroutine

    module subroutine setValueMsg(cmdKeyVal, valueMsg)
      class(KeyValCmdOption), intent(inout) :: cmdKeyVal
      character(len=*),       intent(in)    :: valueMsg
    end subroutine
  end interface

  ! 'PositionalCmdOption' type-bound procedures.
  interface
    module subroutine setPosTypePosition(cmdPosOption, position)
      class(PositionalCmdOption), intent(inout) :: cmdPosOption
      integer,                    intent(in)    :: position
    end subroutine

    module pure function posType_getPosition(self)
      class(PositionalCmdOption), intent(in) :: self
      integer :: posType_getPosition
    end function

    module subroutine assignOptionalPosTypeVal(cmdPosArg, value)
      class(PositionalCmdOption),  intent(inout) :: cmdPosArg
      character(len=LONG_MAX_LEN), intent(in)    :: value
    end subroutine
  end interface

  ! Interface procedures.
  interface
    module subroutine initializeCmdOption(cmdOption, command, altCommand)
      class(BaseCmdOption),       intent(out) :: cmdOption
      character(len=*),           intent(in)  :: command
      character(len=*), optional, intent(in)  :: altCommand
    end subroutine

    module subroutine parsePassedCmdArgs(cmdFlags, cmdKeyVal, cmdPosArgs, &
          readFlag, readKeyVal, readPosArg)
      class(FlagCmdOption),       intent(inout) :: cmdFlags(:)
      class(KeyValCmdOption),     intent(inout) :: cmdKeyVal(:)
      class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
      logical,                    intent(in)    :: readFlag
      logical,                    intent(in)    :: readKeyVal
      logical,                    intent(in)    :: readPosArg
    end subroutine

    module subroutine showHelpMsg(cmdFlags, cmdKeyVal, cmdPosArgs)
      class(FlagCmdOption),       intent(in) :: cmdFlags(:)
      class(KeyValCmdOption),     intent(in) :: cmdKeyVal(:)
      class(PositionalCmdOption), intent(in) :: cmdPosArgs(:)
    end subroutine
  end interface
  ! -------------------------------------------------------------------------- !

  public :: parsePassedCmdArgs
  public :: initializeCmdOption
  public :: showHelpMsg

  public :: setUsageMsg
  public :: setPosTypePosition
  public :: setValueMsg

  public :: assignOptionalPosTypeVal
  public :: assignOptionalKVVal
  public :: assignInitialFlagState
end module CmdOptionType

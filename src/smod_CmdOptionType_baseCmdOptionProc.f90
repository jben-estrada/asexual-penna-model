
submodule (CmdOptionType) baseOptionProcedure
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: baseOptionProcedure
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `CmdOptionType` containing type-bound procedures for the
  !!  abstract type `BaseCmdOption` and all its type extensions.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setUsageMsg
  !>  Set the message on the usage of the provided command-line option.
  !!  This will appear in the help message.
  ! -------------------------------------------------------------------------- !
  subroutine setUsageMsg(cmdOption, usageMsg)
    class(BaseCmdOption), intent(inout) :: cmdOption
      !! Command-line option to modify its usage message.
    character(len=*),     intent(in)    :: usageMsg
      !! The usage message to replace `usageMsg` of `cmdOption`.

    cmdOption % usageMsg = usageMsg
  end subroutine setUsageMsg


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: basecmdtype_getCommand
  !>  Get the command character of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function basecmdtype_getCommand(self) result(command)
    class(BaseCmdOption), intent(in) :: self
      !! Command-line option.
    character(len=LONG_MAX_LEN)      :: command

    command = self % command
  end function basecmdtype_getCommand


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: basecmdtype_getAltCommand
  !>  Get the alternate command character of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function basecmdtype_getAltCommand(self) result(shortCommand)
    class(BaseCmdOption), intent(in) :: self
      !! Command-line option.
    character(len=MAX_LEN)           :: shortCommand

    shortCommand = self % shortCommand
  end function basecmdtype_getAltCommand


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: basecmdtype_getUsageMsg
  !>  Get the message on the usage of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function basecmdtype_getUsageMsg(self) result(usageMsg)
    class(BaseCmdOption), intent(in) :: self
      !! Command-line option.
    character(len=LONG_MAX_LEN)  :: usageMsg

    usageMsg = self % usageMsg
  end function basecmdtype_getUsageMsg


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: basecmdtype_getValue
  !>  Get the assigned value of the given `BaseCmdOption` object.
  ! -------------------------------------------------------------------------- !
  pure function basecmdtype_getValue(self) result(value)
    class(BaseCmdOption), intent(in) :: self

    character(len=LONG_MAX_LEN) :: value

    value = self % value
  end function basecmdtype_getValue
end submodule baseOptionProcedure

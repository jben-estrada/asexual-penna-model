
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
  ! FUNCTION: getCommand
  !>  Get the command character of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function getCommand(self) result(command)
    class(BaseCmdOption), intent(in) :: self
      !! Command-line option.
    character(len=LONG_MAX_LEN)      :: command

    command = self % command
  end function getCommand


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getAltCommand
  !>  Get the alternate command character of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function getAltCommand(self) result(altCommand)
    class(BaseCmdOption), intent(in) :: self
      !! Command-line option.
    character(len=MAX_LEN)           :: altCommand

    altCommand = self % altCommand
  end function getAltCommand


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getUsageMsg
  !>  Get the message on the usage of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function getUsageMsg(self) result(usageMsg)
    class(BaseCmdOption), intent(in) :: self
      !! Command-line option.
    character(len=LONG_MAX_LEN)  :: usageMsg

    usageMsg = self % usageMsg
  end function getUsageMsg
end submodule baseOptionProcedure

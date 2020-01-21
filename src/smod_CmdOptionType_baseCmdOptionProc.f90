
submodule (CmdOptionType) baseOptionProcedure
  implicit none
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setUsageMsg
  !>  Set the message on the usage of the provided command-line option.
  !!  This will appear in the help message.
  ! -------------------------------------------------------------------------- !
  subroutine setUsageMsg(cmdOption, usageMsg)
    class(BaseCmdOption), intent(inout) :: cmdOption
    character(len=*),     intent(in)    :: usageMsg

    cmdOption % usageMsg = usageMsg
  end subroutine setUsageMsg


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCommand
  !>  Get the command character of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function getCommand(self) result(command)
    class(BaseCmdOption), intent(in) :: self
    character(len=LONG_MAX_LEN)      :: command

    command = self % command
  end function getCommand


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getAltCommand
  !>  Get the alternate command character of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function getAltCommand(self) result(altCommand)
    class(BaseCmdOption), intent(in) :: self
    character(len=MAX_LEN)           :: altCommand

    altCommand = self % altCommand
  end function getAltCommand


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getUsageMsg
  !>  Get the message on the usage of the given command-line option.
  ! -------------------------------------------------------------------------- !
  pure function getUsageMsg(self) result(usageMsg)
    class(BaseCmdOption), intent(in) :: self
    character(len=LONG_MAX_LEN)  :: usageMsg

    usageMsg = self % usageMsg
  end function getUsageMsg
end submodule baseOptionProcedure

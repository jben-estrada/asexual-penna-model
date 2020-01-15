
submodule (CmdOptionType) baseOptionProcedure
  implicit none
contains


  subroutine setUsageMsg(cmdOption, usageMsg)
    implicit none
    class(BaseCmdOption), intent(inout) :: cmdOption
    character(len=*),     intent(in)    :: usageMsg

    cmdOption % usageMsg = usageMsg
  end subroutine setUsageMsg


  pure function getCommand(self) result(command)
    implicit none

    class(BaseCmdOption), intent(in) :: self
    character(len=LONG_MAX_LEN)       :: command

    command = self % command
  end function getCommand


  pure function getAltCommand(self) result(altCommand)
    implicit none

    class(BaseCmdOption), intent(in) :: self
    character(len=MAX_LEN)           :: altCommand

    altCommand = self % altCommand
  end function getAltCommand


  pure function getUsageMsg(self) result(usageMsg)
    implicit none

    class(BaseCmdOption), intent(in) :: self
    character(len=LONG_MAX_LEN)  :: usageMsg

    usageMsg = self % usageMsg
  end function getUsageMsg
end submodule baseOptionProcedure

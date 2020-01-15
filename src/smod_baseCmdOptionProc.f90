
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

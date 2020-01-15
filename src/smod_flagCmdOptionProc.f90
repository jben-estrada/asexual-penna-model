
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


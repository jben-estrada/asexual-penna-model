
submodule (CmdOptionType) flagBoundProcedure
  implicit none
contains


  logical pure function flagtype_getFlagState(self)
    implicit none
    class(FlagCmdOption), intent(in) :: self

    flagtype_getFlagState = self % state
  end function flagtype_getFlagState


  subroutine assignInitialFlagState(cmdFlag, state)
    implicit none

    class(FlagCmdOption), intent(inout) :: cmdFlag
    logical,              intent(in)    :: state

    if (cmdFlag % isOptional) then
      print "(3a)", "***ERROR. Cannot assign '", trim(cmdFlag % command), &
          "' its default value again."
      stop
    end if

    cmdFlag % isOptional = .true.
    cmdFlag % state = state
  end subroutine assignInitialFlagState
end submodule flagBoundProcedure


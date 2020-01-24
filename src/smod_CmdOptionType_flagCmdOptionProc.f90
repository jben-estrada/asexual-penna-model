
submodule (CmdOptionType) flagBoundProcedure
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: flagtype_getFlagState
  !>  Get the state of the flag option.
  ! -------------------------------------------------------------------------- !
  logical pure function flagtype_getFlagState(self)
    class(FlagCmdOption), intent(in) :: self
      !! Command-line flag.

    flagtype_getFlagState = self % state
  end function flagtype_getFlagState


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignInitialFlagState
  !>  Assign an initial state to the flag option. By defauly, i.e. without
  !!  invoking this subroutine, the state of all flags are set to false.
  !!  This also marks the flag as optiona.
  ! -------------------------------------------------------------------------- !
  subroutine assignInitialFlagState(cmdFlag, state)
    class(FlagCmdOption), intent(inout) :: cmdFlag
      !! Command-line flag to modify its `state` attribute.
    logical,              intent(in)    :: state
      !! New state to replace `state` of `cmdFlag`.

    if (cmdFlag % isOptional) then
      print "(3a)", "***ERROR. Cannot assign '", trim(cmdFlag % command), &
          "' its default value again."
      stop
    end if

    cmdFlag % isOptional = .true.
    cmdFlag % state = state
  end subroutine assignInitialFlagState
end submodule flagBoundProcedure

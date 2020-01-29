
submodule (CmdOptionType) flagBoundProcedure
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: flagBoundProcedure
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `CmdOptionType` containing type-bound procedures for the
  !!  type `FlagCmdOption`.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: flagtype_getFlagState
  !>  Get the state of the flag option.
  ! -------------------------------------------------------------------------- !
  logical pure function flagtype_getFlagState(self)
    class(FlagCmdOption), intent(in) :: self
      !! Command-line flag.

    flagtype_getFlagState = (self % value == TRUE_FLAG)
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

    if (state) then
      cmdFlag % value = TRUE_FLAG
    else
      cmdFlag % value = FALSE_FLAG
    end if

    cmdFlag % isOptional = .true.
    cmdFlag % hasValue = .true.
  end subroutine assignInitialFlagState
end submodule flagBoundProcedure

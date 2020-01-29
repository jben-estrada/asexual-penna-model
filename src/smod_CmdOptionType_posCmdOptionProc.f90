
submodule (CmdOptionType) posBoundProcedure
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: posBoundProcedure
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `CmdOptionType` containing bound procedures for the type
  !!  `PositionalCmdOption`.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setPosTypePosition
  !>  Set the position of the positional command-line option.
  ! -------------------------------------------------------------------------- !
  subroutine setPosTypePosition(cmdPosOption, position)
    class(PositionalCmdOption), intent(inout) :: cmdPosOption
      !! Positional command-line option.
    integer,                    intent(in)    :: position
      !! Position of `cmdPosOption` in passed positional command-line arguments.
    
    cmdPosOption % position = position 
  end subroutine setPosTypePosition


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: posType_getPosition
  !>  Get the position of the positional command-line option.
  ! -------------------------------------------------------------------------- !
  integer pure function posType_getPosition(self)
    class(PositionalCmdOption), intent(in) :: self
      !! Positional command-line option.

    posType_getPosition = self % position
  end function posType_getPosition


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignOptionalPosTypeVal
  !>  Set the default value of the given positional command-line option.
  !!  This also marks the command-line option as optional.
  ! -------------------------------------------------------------------------- !
  subroutine assignOptionalPosTypeVal(cmdPosArg, value)
    class(PositionalCmdOption),  intent(inout) :: cmdPosArg
      !! Positional command-line option to be modified.
    character(len=LONG_MAX_LEN), intent(in)    :: value
      !! Default value of `cmdPosArg` to be assigned to its `value` attribute.

    if (cmdPosArg % isOptional) then
      print "(3a)", "***ERROR. Cannot assign '", trim(cmdPosArg % command), &
          "' its default value again."
      stop
    end if

    cmdPosArg % hasValue = .true.
    cmdPosArg % isOptional = .true.
    cmdPosArg % value = value
  end subroutine assignOptionalPosTypeVal
end submodule posBoundProcedure

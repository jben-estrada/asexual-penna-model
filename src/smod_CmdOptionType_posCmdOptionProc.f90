
submodule (CmdOptionType) posBoundProcedure
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setPosTypePosition
  !>  Set the position of the positional command-line option.
  ! -------------------------------------------------------------------------- !
  subroutine setPosTypePosition(cmdPosOption, position)
    implicit none

    class(PositionalCmdOption), intent(inout) :: cmdPosOption
    integer,                    intent(in)    :: position
    
    cmdPosOption % position = position 
  end subroutine setPosTypePosition


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: posType_getValue
  !>  Get the value of the positional command-line option.
  ! -------------------------------------------------------------------------- !
  pure function posType_getValue(self) result(value)
    implicit none

    class(PositionalCmdOption), intent(in) :: self
    character(len=LONG_MAX_LEN)            :: value
    
    value = self % value
  end function posType_getValue


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: posType_getPosition
  !>  Get the position of the positional command-line option.
  ! -------------------------------------------------------------------------- !
  integer pure function posType_getPosition(self)
    implicit none
    class(PositionalCmdOption), intent(in) :: self

    posType_getPosition = self % position
  end function posType_getPosition


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignOptionalPosTypeVal
  !>  Set the default value of the given positional command-line option.
  !!  This also marks the command-line option as optional.
  ! -------------------------------------------------------------------------- !
  subroutine assignOptionalPosTypeVal(cmdPosArg, value)
    implicit none

    class(PositionalCmdOption),  intent(inout) :: cmdPosArg
    character(len=LONG_MAX_LEN), intent(in)    :: value

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


submodule (CmdOptionType) posBoundProcedure
  implicit none
contains


  subroutine setPosTypePosition(cmdPosOption, position)
    implicit none

    class(PositionalCmdOption), intent(inout) :: cmdPosOption
    integer,                    intent(in)    :: position
    
    cmdPosOption % position = position 
  end subroutine setPosTypePosition


  pure function posType_getValue(self) result(value)
    implicit none

    class(PositionalCmdOption), intent(in) :: self
    character(len=LONG_MAX_LEN)            :: value
    
    value = self % value
  end function posType_getValue


  integer pure function posType_getPosition(self)
    implicit none
    class(PositionalCmdOption), intent(in) :: self

    posType_getPosition = self % position
  end function posType_getPosition


  subroutine assignOptionalPosTypeVal(cmdPosArg, value)
    implicit none

    class(PositionalCmdOption),  intent(inout) :: cmdPosArg
    character(len=LONG_MAX_LEN), intent(in)    :: value

    cmdPosArg % hasValue = .true.
    cmdPosArg % isOptional = .true.
    cmdPosArg % value = value
  end subroutine assignOptionalPosTypeVal
end submodule posBoundProcedure

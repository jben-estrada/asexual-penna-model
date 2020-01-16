
submodule (CmdOptionType) KVBoundProcedure
  implicit none
contains


  pure integer function KVtype_getValue(self)
    implicit none
    class(KeyValCmdOption), intent(in) :: self

    KVtype_getValue = self % value
  end function KVtype_getValue


  subroutine assignOptionalKVVal(cmdKeyVal, value)
    implicit none

    class(KeyValCmdOption), intent(inout) :: cmdKeyVal
    integer,                intent(in)    :: value

    cmdKeyVal % hasValue = .true.
    cmdKeyVal % isOptional = .true.
    cmdKeyVal % value = value
  end subroutine assignOptionalKVVal
end submodule KVBoundProcedure

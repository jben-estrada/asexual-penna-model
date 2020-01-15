
submodule (CmdOptionType) KVBoundProcedure
  implicit none
contains


  logical function KVtype_allocatedValue(self)
    implicit none
    class(KeyValCmdOption), intent(in) :: self
    
    KVtype_allocatedValue = allocated(self % value)
  end function KVtype_allocatedValue


  integer function KVtype_getValue(self)
    implicit none
    class(KeyValCmdOption), intent(in) :: self

    if (allocated(self % value)) then
      KVtype_getValue = self % value
    else
      print "(3a)", "***ERROR. '", self % getCommand(), "' has no value."
      stop
    end if
  end function KVtype_getValue


  subroutine KVtype_setValue(self, value)
    implicit none
    class(KeyValCmdOption), intent(inout) :: self
    integer,                intent(in)    :: value

    ! NOTE: Automatic allocation.
    self % value = value
  end subroutine KVtype_setValue


  subroutine finalizeKVObj(self)
    implicit none
    type(KeyValCmdOption), intent(inout) :: self

    if (allocated(self % command)) deallocate(self % command)
    if (allocated(self % altCommand)) deallocate(self % altCommand)
    if (allocated(self % value)) deallocate(self % value)
  end subroutine finalizeKVObj
end submodule KVBoundProcedure

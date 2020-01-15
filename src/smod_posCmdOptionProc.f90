
submodule (CmdOptionType) posBoundProcedure
  implicit none
contains


  logical function posType_allocatedValue(self)
    implicit none
    class(PositionalCmdOption), intent(in) :: self
    
    posType_allocatedValue = allocated(self % value)
  end function posType_allocatedValue


  function posType_getValue(self) result(value)
    implicit none
    class(PositionalCmdOption), intent(in) :: self

    character(len=:), allocatable :: value
    
    if (allocated(self % value)) then
      value = self % value
    else
      print "(3a)", "***ERROR. '", self % getCommand(), "' has no value."
      stop
    end if
  end function posType_getValue


  subroutine posType_setValue(self, value)
    implicit none

    class(PositionalCmdOption), intent(inout) :: self
    character(len=*),           intent(in)    :: value

    ! NOTE: Automatic allocation.
    self % value = value
  end subroutine posType_setValue


  subroutine posType_setPosition(self, position)
    implicit none

    class(PositionalCmdOption), intent(inout) :: self
    integer,                    intent(in)    :: position
    
    self % position = position 
  end subroutine posType_setPosition


  integer function posType_getPosition(self)
    implicit none
    class(PositionalCmdOption), intent(inout) :: self

    posType_getPosition = self % position
  end function posType_getPosition


  subroutine finalizePosObj(self)
    implicit none
    type(PositionalCmdOption), intent(inout) :: self

    if (allocated(self % command)) deallocate(self % command)
    if (allocated(self % altCommand)) deallocate(self % altCommand)
    if (allocated(self % value)) deallocate(self % value)
  end subroutine finalizePosObj
end submodule posBoundProcedure

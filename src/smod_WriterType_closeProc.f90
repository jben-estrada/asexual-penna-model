

submodule (WriterType) WriterTypeCloseProc
  !----------------------------------------------------------------------------!
  ! SUBMODULE: WriterTypeCloseProc
  !>  Submodule containing the specific procedures for the generic
  !!  type-bound procedure `[Writer]%close`.
  !----------------------------------------------------------------------------!
  implicit none
  contains
  module subroutine writer_closeAll(self)
    implicit none
    class(Writer), intent(inout) :: self

    integer :: flag
    integer :: i

    do i = 1, size(self%liveFlags)
      flag = self%liveFlags(i)
      close(units(flag))
    end do

    if (allocated(self%liveFlags)) then
      deallocate(self%liveFlags)
      allocate(self%liveFlags(0))
    end if
  end subroutine writer_closeAll


  module subroutine writer_close(self, flag)
    implicit none

    class(Writer), intent(inout) :: self
    integer,       intent(in)    :: flag

    if (.not.any(self%liveFlags == flag)) then
      print "(a, i2)", "Chosen flag is not initialized! flag: ", flag
      return
    end if

    close(units(flag))
    call arrayRemoveElem(self%enabledFlags, flag)
  end subroutine writer_close


  module subroutine writer_listclose(self, flags)
    implicit none

    class(Writer), intent(inout) :: self
    integer,       intent(in)    :: flags(:)

    integer :: i

    do i = 1, size(flags)
      if (.not.any(self%liveFlags == flags(i))) then
        print "(a, i2)", "***WARNING. Chosen flag is not initialized! flag: ", &
            flags(i)
      else
        close(units(flags(i)))
        call arrayRemoveElem(self%enabledFlags, flags(i))
      end if
    end do
  end subroutine writer_listclose
end submodule

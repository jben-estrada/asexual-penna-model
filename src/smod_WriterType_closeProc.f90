

submodule (WriterType) WriterTypeCloseProc
  !----------------------------------------------------------------------------!
  ! SUBMODULE: WriterTypeCloseProc
  !>  Submodule containing the specific procedures for the generic
  !!  type-bound procedure `[Writer]%close`.
  !----------------------------------------------------------------------------!
  implicit none
  contains

  
  subroutine writer_closeAll(self)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.

    integer :: flag
    integer :: i

    do i = 1, size(self%liveFlags)
      flag = self%liveFlags(i)
      close(unitArray(flag))
    end do

    if (allocated(self%liveFlags)) then
      deallocate(self%liveFlags)
      allocate(self%liveFlags(0))
    end if
  end subroutine writer_closeAll


  subroutine writer_close(self, flag)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    integer,       intent(in)    :: flag
      !! Integer flag whose corresponding output file is to be closed if active.

    if (.not.any(self%liveFlags == flag)) then
      print "(a, i2)", "Chosen flag is not initialized! flag: ", flag
      return
    end if

    close(unitArray(flag))
    call arrayRemoveElem(self%enabledFlags, flag)
  end subroutine writer_close


  subroutine writer_listclose(self, flags)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    integer,       intent(in)    :: flags(:)
      !! Array of integer flags whose corresponding output files are to be 
      !! closed if active.

    integer :: i

    do i = 1, size(flags)
      if (.not.any(self%liveFlags == flags(i))) then
        print "(a, i2)", "***WARNING. Chosen flag is not initialized! flag: ", &
            flags(i)
      else
        close(unitArray(flags(i)))
        call arrayRemoveElem(self%enabledFlags, flags(i))
      end if
    end do
  end subroutine writer_listclose
end submodule

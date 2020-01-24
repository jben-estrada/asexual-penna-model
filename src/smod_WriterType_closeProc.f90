

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
    integer :: i

    if (allocated(self % liveFiles)) then
      ! Close all active output files.
      do i = 1, size(self % liveFiles)
        close(self % liveFiles(i) % unit)
      end do

      ! Empty the `liveFiles` attribute.
      deallocate(self % liveFiles)
      allocate(self % liveFiles(0))
    end if
  end subroutine writer_closeAll


  subroutine writer_close(self, flag)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    integer,       intent(in)    :: flag
      !! Integer flag whose corresponding output file is to be closed if active.

    type(OutputFile), allocatable :: foundFile

    call findFileByFlag(self % liveFiles, flag, foundFile)
  end subroutine writer_close


  subroutine writer_listclose(self, flags)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    integer,       intent(in)    :: flags(:)
      !! Array of integer flags whose corresponding output files are to be 
      !! closed if active.

    type(OutputFile), allocatable :: foundFile
    integer :: i

    ! Search for the files to remove.
    do i = 1, size(flags)
      call findFileByFlag(self % liveFiles, flags(i), foundFile)

      ! Remove file to remove.
      if (allocated(foundFile)) &
         call removeFilebyFlag(self % liveFiles, foundFile % flag)
    end do
  end subroutine writer_listclose
end submodule

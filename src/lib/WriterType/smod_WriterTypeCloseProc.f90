submodule (WriterType) WriterTypeCloseProc
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: WriterTypeCloseProc
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `WriterType` containing the specific procedures for the
  !!  generic type-bound procedure `[Writer] % close`.
  ! -------------------------------------------------------------------------- !
  implicit none
  contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_closeAll
  !>  Close all active files, i.e. `OPEN`ed files, available to `self`. This is
  !!  one of the specific procedures to the bound procedure `close` of `Writer`
  !!  objects.
  ! -------------------------------------------------------------------------- !
  subroutine writer_closeAll(self)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    integer :: i

    if (allocated(self % activeFiles)) then
      ! Close all active output files.
      do i = 1, size(self % activeFiles)
        close(self % activeFiles(i) % unit)
      end do

      ! Empty the `activeFiles` attribute.
      deallocate(self % activeFiles)
      allocate(self % activeFiles(0))
    end if
  end subroutine writer_closeAll


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_close
  !>  Close a specific active files, i.e. an `OPEN`ed file, available to `self`.
  !!  This is one of the specific procedures to the bound procedure `close` of
  !!  `Writer` objects.
  ! -------------------------------------------------------------------------- !
  subroutine writer_close(self, flag)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    character,     intent(in)    :: flag
      !! Integer flag whose corresponding output file is to be closed if active.

    type(OutputFile), allocatable :: foundFile

    call findFileByFlag(self % activeFiles, flag, foundFile)
  end subroutine writer_close


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_listclose
  !>  Close list of specific active files, i.e. `OPEN`ed files, available to
  !!  `self`. This is one of the specific procedures to the bound procedure
  !!  `close` of `Writer` objects.
  ! -------------------------------------------------------------------------- !
  subroutine writer_listclose(self, flags)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    character,     intent(in)    :: flags(:)
      !! Array of integer flags whose corresponding output files are to be 
      !! closed if active.

    type(OutputFile), allocatable :: foundFile
    integer :: i

    ! Search for the files to remove.
    do i = 1, size(flags)
      call findFileByFlag(self % activeFiles, flags(i), foundFile)

      ! Remove file to remove.
      if (allocated(foundFile)) &
         call removeFilebyFlag(self % activeFiles, foundFile % flag)
    end do
  end subroutine writer_listclose
end submodule

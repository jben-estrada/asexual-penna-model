submodule (WriterType) WriterTypeWrHdrProc
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: WriterTypeWrHdrProc
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `WriterType` containing the specific procedures for the
  !!  generic type-bound procedure `[Writer] % writeHeader`.
  ! -------------------------------------------------------------------------- !
  implicit none
  contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_writeHeader
  !>  Write the header of the active file specified by the integer `flag`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_writeHeader(self, flag, header)
    class(Writer),    intent(in) :: self
      !! `Writer` object.
    character,        intent(in) :: flag
      !! Flag corresponding to an output file to be written on.
    character(len=*), intent(in) :: header(:)
      !! Headers to describe sets of data.

    logical :: flagFound
    integer :: i, j

    ! Do linear search for the file to write on.
    flagFound = .false.
    do i = 1, size(self % activeFiles)

      if (self % activeFiles(i) % flag == flag) then
        write(self % activeFiles(i) % unit, "(*(a, ', '))") &
          (trim(header(j)), j = 1, size(header))

        flagFound = .true.
        exit
      end if
    end do

    if (.not. flagFound) then
      print "(3a)", "***ERROR. Cannot write header, flag ('", flag, &
          "') not found."
      stop
    end if
  end subroutine writer_writeHeader
end submodule

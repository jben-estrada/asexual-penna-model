submodule (WriterType) WriterTypeWrHdrProc
  implicit none
  contains


  subroutine writer_writeHeader(self, flag, header)
    class(Writer),    intent(in) :: self
      !! `Writer` object.
    integer,          intent(in) :: flag
      !! Flag corresponding to an output file to be written on.
    character(len=*), intent(in) :: header(:)
      !! Headers to describe sets of data.

    logical :: flagFound
    integer :: i, j

    ! Do linear search for the file to write on.
    flagFound = .false.
    do i = 1, size(self % liveFiles)

      if (self % liveFiles(i) % flag == flag) then
        write(self % liveFiles(i) % unit, "(*(a, ', '))") &
          (trim(header(j)), j = 1, size(header))

        flagFound = .true.
        exit
      end if
    end do

    if (.not. flagFound) then
      print "(a, i0, a)", "***ERROR. Cannot write header, flag (", flag, &
          ") not found."
      stop
    end if
  end subroutine writer_writeHeader
end submodule

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

    integer :: i

    if (.not.any(self%liveFlags == flag)) return
    do i = 1, size(header)
      write(unitArray(flag), "(a, ', ')", advance="no") trim(header(i))
    end do
    
    ! Print new line.
    write(unitArray(flag), *) ""
  end subroutine writer_writeHeader
end submodule

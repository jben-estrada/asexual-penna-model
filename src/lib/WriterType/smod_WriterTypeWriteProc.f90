
submodule (WriterType) WriterTypeWriteProc
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: WriterTypeWriteProc
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `WriterType` containing the specific procedures for the
  !!  generic type-bound procedure `[Writer] % write`.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_int
  !>  Write an integer data into an active file specified by the integer 
  !!  `flag`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_int(self, flag, arg)
    class(Writer),         intent(inout) :: self
      !! `Writer` object.
    integer(kind=writeIK), intent(in)    :: arg
      !! A data point to be written on the output file.
    character,               intent(in)  :: flag
      !! Flag corresponding to the output file to be written on.

    type(OutputFile), allocatable :: foundFile

    call findFileByFlag(self % activeFiles, flag, foundFile)

    if (allocated(foundFile)) then
      write(foundFile % unit, foundFile % format) arg
    else
      call raiseError("Cannot write file, The flag '" // flag // &
        "' does not correspond to any recordable data.")
    end if
  end subroutine writer_write_int


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_real
  !>  Write a real number data into an active file specified by the integer
  !!  `flag`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_real(self, flag, arg)
    class(Writer),      intent(inout) :: self
      !! `Writer` object.
    real(kind=writeRK), intent(in)    :: arg
      !! A data point to be written on the output file.
    character,          intent(in)    :: flag
      !! Flag corresponding to the output file to be written on.

    type(OutputFile), allocatable :: foundFile

    call findFileByFlag(self % activeFiles, flag, foundFile)

    if (allocated(foundFile)) then
      write(foundFile % unit, foundFile % format) arg
    else
      call raiseError("Cannot write file, The flag '" // flag // &
        "' does not correspond to any recordable data.")
    end if
  end subroutine writer_write_real


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_intArray
  !>  Write an array of integer data into an active file specified by the 
  !!  integer `flag`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_intArray(self, flag, arg)
    class(Writer),         intent(inout) :: self
      !! `Writer` object.
    integer(kind=writeIK), intent(in)    :: arg(:)
      !! Data to be written on the output file.
    character,             intent(in)    :: flag
      !! Flag corresponding to the output file to be written on.

    type(OutputFile), allocatable :: foundFile

    call findFileByFlag(self % activeFiles, flag, foundFile)

    if (allocated(foundFile)) then
      write(foundFile % unit, foundFile % format) arg
    else
      call raiseError("Cannot write file, The flag '" // flag // &
        "' does not correspond to any recordable data.")
    end if
  end subroutine writer_write_intArray


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_realArray
  !>  Write an array of real number data into an active file specified by the 
  !!  integer `flag`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_realArray(self, flag, arg)
    class(Writer),      intent(inout) :: self
      !! `Writer` object.
    real(kind=writeRK), intent(in)    :: arg(:)
      !! Data to be written on the output file.
    character,          intent(in)    :: flag
      !! Flag corresponding to the output file to be written on.

    type(OutputFile), allocatable :: foundFile

    call findFileByFlag(self % activeFiles, flag, foundFile)

    if (allocated(foundFile)) then
      write(foundFile % unit, foundFile % format) arg
    else
      call raiseError("Cannot write file, The flag '" // flag // &
        "' does not correspond to any recordable data.")
    end if
  end subroutine writer_write_realArray
end submodule WriterTypeWriteProc

submodule (WriterType) WriterTypeWriteProc
  !----------------------------------------------------------------------------!
  ! SUBMODULE: WriterTypeWriteProc
  !>  Submodule containing the specific procedures for the generic
  !!  type-bound procedure `[Writer]%write`.
  !----------------------------------------------------------------------------!
  implicit none
  contains


  subroutine writer_write_int(self, flag, arg)
    class(Writer),         intent(inout) :: self
      !! `Writer` object.
    integer(kind=writeIK), intent(in)    :: arg
      !! A data point to be written on the output file.
    integer,               intent(in)    :: flag
      !! Flag corresponding to the output file to be written on.

    if (.not.any(self%liveFlags == flag)) return
    write(unitArray(flag), formatArray(flag)) arg
  end subroutine writer_write_int


  subroutine writer_write_real(self, flag, arg)
    class(Writer),      intent(inout) :: self
      !! `Writer` object.
    real(kind=writeRK), intent(in)    :: arg
      !! A data point to be written on the output file.
    integer,            intent(in)    :: flag
      !! Flag corresponding to the output file to be written on.

    if (.not.any(self%liveFlags == flag)) return

    write(unitArray(flag), formatArray(flag)) arg
  end subroutine writer_write_real


  subroutine writer_write_intArray(self, flag, arg)
    class(Writer),         intent(inout) :: self
      !! `Writer` object.
    integer(kind=writeIK), intent(in)    :: arg(:)
      !! Data to be written on the output file.
    integer,               intent(in)    :: flag
      !! Flag corresponding to the output file to be written on.

    if (.not.any(self%liveFlags == flag)) return

    write(unitArray(flag), formatArray(flag)) arg
  end subroutine writer_write_intArray


  subroutine writer_write_realArray(self, flag, arg)
    class(Writer),      intent(inout) :: self
      !! `Writer` object.
    real(kind=writeRK), intent(in)    :: arg(:)
      !! Data to be written on the output file.
    integer,            intent(in)    :: flag
      !! Flag corresponding to the output file to be written on.

    if (.not.any(self%liveFlags == flag)) return

    write(unitArray(flag), formatArray(flag)) arg
  end subroutine writer_write_realArray
end submodule WriterTypeWriteProc

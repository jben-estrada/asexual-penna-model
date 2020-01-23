
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
    integer(kind=writeIK), intent(in)    :: arg
    integer,               intent(in)    :: flag

    if (.not.any(self%liveFlags == flag)) return
    write(unitArray(flag), formatArray(flag)) arg
  end subroutine writer_write_int


  subroutine writer_write_real(self, flag, arg)
    class(Writer),      intent(inout) :: self
    integer,            intent(in)    :: flag
    real(kind=writeRK), intent(in)    :: arg

    if (.not.any(self%liveFlags == flag)) return

    write(unitArray(flag), formatArray(flag)) arg
  end subroutine writer_write_real


  subroutine writer_write_intArray(self, flag, arg)
    class(Writer),         intent(inout) :: self
    integer,               intent(in)    :: flag
    integer(kind=writeIK), intent(in)    :: arg(:)

    if (.not.any(self%liveFlags == flag)) return

    write(unitArray(flag), formatArray(flag)) arg
  end subroutine writer_write_intArray


  subroutine writer_write_realArray(self, flag, arg)
    class(Writer),      intent(inout) :: self
    integer,            intent(in)    :: flag
    real(kind=writeRK), intent(in)    :: arg(:)

    if (.not.any(self%liveFlags == flag)) return

    write(unitArray(flag), formatArray(flag)) arg
  end subroutine writer_write_realArray
end submodule WriterTypeWriteProc


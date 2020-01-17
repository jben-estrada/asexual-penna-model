
submodule (WriterType) WriterTypeWriteProc
  !----------------------------------------------------------------------------!
  ! SUBMODULE: WriterTypeWriteProc
  !>  Submodule containing the specific procedures for the generic
  !!  type-bound procedure `[Writer]%write`.
  !----------------------------------------------------------------------------!
  implicit none
  contains
  module subroutine writer_write_int(self, flag, arg)
    implicit none

    class(Writer),         intent(inout) :: self
    integer(kind=writeIK), intent(in)    :: arg
    integer,               intent(in)    :: flag

    if (.not.any(self%liveFlags == flag)) return
    write(units(flag), formats(flag)) arg
  end subroutine writer_write_int


  module subroutine writer_write_real(self, flag, arg)
    implicit none

    class(Writer),      intent(inout) :: self
    integer,            intent(in)    :: flag
    real(kind=writeRK), intent(in)    :: arg

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_real


  module subroutine writer_write_intArray(self, flag, arg)
    implicit none

    class(Writer),         intent(inout) :: self
    integer,               intent(in)    :: flag
    integer(kind=writeIK), intent(in)    :: arg(:)

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_intArray


  module subroutine writer_write_realArray(self, flag, arg)
    implicit none

    class(Writer),      intent(inout) :: self
    integer,            intent(in)    :: flag
    real(kind=writeRK), intent(in)    :: arg(:)

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_realArray
end submodule WriterTypeWriteProc


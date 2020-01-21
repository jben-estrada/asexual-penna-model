
submodule (WriterType) WriterTypeInitProc
  !----------------------------------------------------------------------------!
  ! SUBMODULE: WriterTypeInitProc
  !>  Submodule containing the specific procedures for the generic
  !!  type-bound procedure `[Writer]%initialize`.
  !----------------------------------------------------------------------------!
  implicit none
  contains


  subroutine initializeWriter(filename, unit, position)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: position
    integer,          intent(in) :: unit

    logical :: exists

    inquire(file=filename, exist=exists)

    if (exists) then
      open(unit, file=filename, status="old", position=position)
    else
      open(unit, file=filename, status="new")
    end if
  end subroutine initializeWriter


  subroutine writer_initializeAll(self)
    class(Writer), intent(inout) :: self

    integer :: flag
    integer :: i

    if (size(self%enabledFlags) == 0) return

    ! Put all enabled flags into live flags
    if(allocated(self%liveFlags)) deallocate(self%liveFlags)
    allocate(self%liveFlags(size(self%enabledFlags)))

    self%liveFlags = self%enabledFlags  ! Hopefully, just a copy.

    do i = 1, size(self%enabledFlags)
      flag = self%enabledFlags(i)
      call initializeWriter(filenames(flag), units(flag), positions(flag))
    end do
  end subroutine writer_initializeAll


  subroutine writer_initialize(self, flag)
    class(Writer), intent(inout) :: self
    integer, intent(in)          :: flag

    if (.not.any(self%enabledFlags == flag)) return

    ! Put enabled flag to live flag
    if(allocated(self%liveFlags)) deallocate(self%liveFlags)
    allocate(self%liveFlags(1))

    self%liveFlags = [flag]  ! NOTE: Automatic allocation

    call initializeWriter(filenames(flag), units(flag), positions(flag))
  end subroutine writer_initialize


  subroutine writer_listInitialize(self, flags)
    class(Writer), intent(inout) :: self
    integer, intent(in)          :: flags(:)

    integer :: i
    integer :: flag

    if(allocated(self%liveFlags)) deallocate(self%liveFlags)
    allocate(self%liveFlags(0))

    do i = 1, size(flags)
      flag = flags(i)
      if (.not.any(self%enabledFlags == flag)) cycle
      call arrayInsert(self%liveFlags, 1, flag)
      call initializeWriter(filenames(flag), units(flag), positions(flag))
    end do
  end subroutine writer_listInitialize
end submodule WriterTypeInitProc

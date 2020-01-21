submodule (WriterType) WriterTypeInterfaceProc
  implicit none
  contains


  ! === `Writer` CONSTRUCTOR SPECIFIC PROCEDURES ===
  subroutine constructWriter_array(new, flags, initialize)
    type(Writer),      intent(out) :: new
    integer,           intent(in)  :: flags(:)
    logical, optional, intent(in)  :: initialize

    integer      :: i
    integer      :: flag

    allocate(new%enabledFlags(0))
    allocate(new%liveFlags(0))

    do i = 1, size(flags)
      flag = flags(i)
      if (flag <= 0 .or. flag > fileCount) then
        print "(a, i3)", "Invalid flag! flag: ", flag
      else
        call arrayInsert(new%enabledFlags, 1, flag)
      end if
    end do

    if (present(initialize)) then
      if (initialize) then
        call new%initialize
      end if
    end if
  end subroutine constructWriter_array


  subroutine constructWriter_scalar(new, flag, initialize)
    type(Writer),      intent(out) :: new
    integer,           intent(in)  :: flag
    logical, optional, intent(in) :: initialize


    allocate(new%enabledFlags(0))
    allocate(new%liveFlags(0))

    if (flag <= 0 .or. flag > fileCount) then
      print "(a, i3)", "Invalid flag! flag: ", flag
      return
    else
      call arrayInsert(new%enabledFlags, 1, flag)
    end if

    if (present(initialize)) then
      if (initialize) then
        call new%initialize
      end if
    end if
  end subroutine constructWriter_scalar


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]destroy
  !>  Deallocate the allocatable attributes `enabledFlags`
  !!  and `liveFlags`.
  !----------------------------------------------------------------------------!
  subroutine destroy(self)
    type(Writer), intent(inout) :: self

    if (allocated(self%enabledFlags)) deallocate(self%enabledFlags)
    if (allocated(self%liveFlags)) deallocate(self%liveFlags)
  end subroutine destroy
end submodule
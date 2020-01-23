submodule (WriterType) WriterTypeInterfaceProc
  implicit none
  contains


  subroutine constructWriter_array(new, flags, initialize)
    type(Writer),      intent(out) :: new
      !! Newly initialized `Writer` object. 
    integer,           intent(in)  :: flags(:)
      !! Flags corresponding to output files available to be written on.
    logical, optional, intent(in)  :: initialize
      !! Initialize `new` with all its available output files set to active.

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
      !! Newly initialized `Writer` object.
    integer,           intent(in)  :: flag
      !! Flag corresponding to output files available to be written on.
    logical, optional, intent(in) :: initialize
      !! Initialize `new` with all its available output files set to active.

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
      !! `Writer` object to be destroyed.

    if (allocated(self%enabledFlags)) deallocate(self%enabledFlags)
    if (allocated(self%liveFlags)) deallocate(self%liveFlags)
  end subroutine destroy
end submodule
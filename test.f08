program name
  implicit none
  type :: link
    integer :: val = 0
    type(link), pointer :: next => null()
  end type link

  type(link), pointer :: head_ptr 
  type(link), pointer :: temp_ptr

  allocate(head_ptr)

  call extend_list(head_ptr, 5)

  temp_ptr => head_ptr
  do
    if (associated(temp_ptr)) then
      print *, temp_ptr%val
      temp_ptr => temp_ptr%next
    else
      exit
    end if
  end do
contains
  
  subroutine extend_list(node_ptr, val)
    implicit none
    type(link), pointer, intent(inout) :: node_ptr
    integer, intent(in) :: val

    integer :: i
    type(link), pointer :: new_ptr, old_ptr

    new_ptr => null()
    old_ptr => node_ptr

    do i = 1, val
      allocate(new_ptr)
      old_ptr%next => new_ptr
      old_ptr => new_ptr

      new_ptr%val = i
    end do
  end subroutine extend_list
end program name

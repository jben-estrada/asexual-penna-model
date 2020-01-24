submodule (UpdateArray) RemoveProcedures
  implicit none
contains


  subroutine arrayRemove_int(array,  k)
    integer(kind=arrIK), allocatable, intent(inout) :: array(:)
      !! 1-dimensional array of integers to be modified.
    integer,                          intent(in)    :: k
      !! Index of the element in `array` to be removed.

    integer(kind=arrIK), allocatable :: temp(:)
    integer :: old_size
    integer :: error

    ! Move contents of `array` to a temporary array.
    old_size = size(array)
    call move_alloc(array, temp)

    if (k > old_size .or. k < 1) then
      print "(2(a, i0))", "***ERROR. ", k, " > ", old_size
      error stop
    end if

    ! Reassign elements of `array` w/o the kth element.
    allocate(array(old_size - 1), stat=error)
    call allocCheck(error)
    array(1:k-1) = temp(1:k-1)
    array(k:old_size-1) = temp(k+1:size(temp))
    
    deallocate(temp)
  end subroutine arrayRemove_int


  subroutine arrayRemove_real(array,  k)
    real(kind=arrRK), allocatable, intent(inout) :: array(:)
      !! 1-dimensional array of real numbers to be modified.
    integer,                       intent(in)    :: k
      !! Index of the element in `array` to be removed.

    real(kind=arrRK), allocatable :: temp(:)
    integer :: old_size
    integer :: error

    ! Move contents of `array` to a temporary array.
    old_size = size(array)
    call move_alloc(array, temp)

    if (k > old_size .or. k < 1) then
      print "(2(a, i0))", "***ERROR. ", k, " > ", old_size
      error stop
    end if

    ! Reassign elements of `array` w/o the kth element.
    allocate(array(old_size - 1), stat=error)
    call allocCheck(error)
    array(1:k-1) = temp(1:k-1)
    array(k:old_size-1) = temp(k+1:size(temp))
    
    deallocate(temp)
  end subroutine arrayRemove_real


  
  subroutine arrayRemoveRange_int(array, a, b)
    integer(kind=arrIK), allocatable, intent(inout) :: array(:)
      !! 1-dimensional array of integers to be modified.
    integer(kind=arrIK),              intent(in)    :: a
      !! The inclusive lower bound of the sub-array in `array` to be removed.
    integer,                          intent(in)    :: b
    !! The inclusive upper bound of the sub-array in `array` to be removed.

    integer(kind=arrIK), allocatable :: temp(:)
    integer :: old_size
    integer :: new_size
    integer :: error

    if (a > b .or. a < 1) then
      print "(2(a, i0), a)", "***ERROR. Improbable range: [", a, " ,", b, "]" 
      error stop
    end if

    if (a > size(array)) then
      print "(a, i0)", "***ERROR. Index out of range: ",  a
      error stop
    else if (b > size(array)) then
      print "(a, i0)", "***ERROR. Index out of range: ",  b
      error stop
    end if
  
    ! Move contents of `array` into a temporary array.
    old_size = size(array)
    new_size = old_size - (b - a) - 1
    call move_alloc (array, temp)

    ! Reassign elements to `array` w/o the elements of index `a` to `b`
    allocate (array(new_size), stat=error)
    call allocCheck(error)
    array(1:a-1) = temp(1:a-1)
    array(a:new_size) = temp(b+1:size(temp))

    deallocate(temp)
  end subroutine arrayRemoveRange_int


  subroutine arrayRemoveRange_real(array, a, b)
    real(kind=arrRK), allocatable, intent(inout) :: array(:)
      !! 1-dimensional array of real numbers to be modified.
    integer,                       intent(in)    :: a
    !! The inclusive lower bound of the sub-array in `array` to be removed.
    integer,                       intent(in)    :: b
    !! The inclusive upper bound of the sub-array in `array` to be removed.

    real(kind=arrRK), allocatable :: temp(:)
    integer :: old_size
    integer :: new_size
    integer :: error

    if (a > b .or. a < 1) then
      print "(2(a, i0), a)", "***ERROR. Improbable range: [", a, " ,", b, "]" 
      error stop
    end if

    if (a > size(array)) then
      print "(a, i0)", "***ERROR. Index out of range: ",  a
      error stop
    else if (b > size(array)) then
      print "(a, i0)", "***ERROR. Index out of range: ",  b
      error stop
    end if

    ! Move contents of `array` into a temporary array.
    old_size = size(array)
    new_size = old_size - (b - a) - 1
    call move_alloc (array, temp)

    ! Reassign elements to `array` w/o the elements of index `a` to `b`
    allocate (array(new_size), stat=error)
    call allocCheck(error)
    array(1:a-1) = temp(1:a-1)
    array(a:new_size) = temp(b+1:size(temp))

    deallocate(temp)
  end subroutine arrayRemoveRange_real


  subroutine arrayRemoveElem_int(array, elem)
    integer(kind=arrIK), allocatable, intent(inout) :: array(:)
      !! 1-dimensional array of integers to be modified.
    integer(kind=arrIK),              intent(in)    :: elem
      !! The integer in `array` to be removed its first instance.

    integer :: i

    ! Do linear search.
    do i = 1, size(array)
      if (array(i) == elem) then
        call arrayRemove_int(array, i)
        return
      end if
    end do
  end subroutine arrayRemoveElem_int


  subroutine arrayRemoveElem_real(array, elem)
    real(kind=arrRK), allocatable, intent(inout) :: array(:)
      !! 1-dimensional array of real numbers to be modified.
    real(kind=arrRK),              intent(in)    :: elem
      !! The real number in `array` to be removed its first instance.
    integer :: i

    ! Tolerance value. Two real numbers are considered 'equal' if
    ! their difference is less that the tolerance value.
    real(kind=arrRK), parameter :: realTolerance = 1e-10

    do i = 1, size(array)
      if (abs(array(i) - elem) < realTolerance) then
        call arrayRemove_real(array, i)
        return
      end if
    end do
  end subroutine arrayRemoveElem_real
end submodule

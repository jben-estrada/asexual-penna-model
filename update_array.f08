module UpdateArray
  ! -------------------------------------------------------------------------- !
  ! MODULE: UpdateArray
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for array manipulations which involve
  !   allocations and deallocations.
  ! -------------------------------------------------------------------------- !
  use iso_fortran_env, only: real64
  implicit none
  private

  ! -------------------------------------------------------------------------- !
  ! GENERIC SUBROUTINE: arrayInsert
  !>  Insert a number (either integer4 (int32) or real8 (int64)) of either rank
  !   0 or 1 at the `k`th index of an array of same type. 
  ! -------------------------------------------------------------------------- !
  interface arrayInsert
    procedure :: arrayInsert_int
    procedure :: arrayInsert_intRange
    procedure :: arrayInsert_real
    procedure :: arrayInsert_realRange
  end interface arrayInsert

  ! -------------------------------------------------------------------------- !
  ! GENERIC SUBROUTINE: generateIndices
  !>  Remove the `k`th element of an array.
  ! -------------------------------------------------------------------------- !
  interface arrayRemove
    procedure :: arrayRemove_int
    procedure :: arrayRemove_real
  end interface arrayRemove

  ! -------------------------------------------------------------------------- !
  ! GENERIC SUBROUTINE: arrayRemoveRange
  !>  Remove a range of elements from an array.
  ! -------------------------------------------------------------------------- !
  interface arrayRemoveRange
    procedure :: arrayRemoveRange_int
    procedure :: arrayRemoveRange_real
  end interface arrayRemoveRange

  ! -------------------------------------------------------------------------- !
  ! GENERIC SUBROUTINE: arrayRemoveElem
  !>  Remove the specified element from an array. If the element is not found,
  !   the subroutine passes silently.
  ! -------------------------------------------------------------------------- !
  interface arrayRemoveElem
    procedure :: arrayRemoveElem_int
    procedure :: arrayRemoveElem_real
  end interface arrayRemoveElem

  public :: arrayInsert
  public :: arrayRemove
  public :: arrayRemoveRange
  public :: arrayRemoveElem
  public :: alloc_check
contains

  ! === ARRAY INSERT SPECIFIC PROCEDURES===
  subroutine arrayInsert_int(array, k, newElem)
    implicit none
    integer, allocatable, intent(inout):: array(:)
    integer, intent(in) :: newElem
    integer, intent(in) :: k

    integer, allocatable  :: temp(:)
    integer :: old_size
    integer :: error

    old_size = size(array)
    call move_alloc(array, temp)

    if (k < 1) then
      print *, "STOP", k
      return
    end if

    allocate(array(old_size + 1), stat=error)
    call alloc_check(error)
    array(1:k-1) = temp(1:k-1)
    array(k) = newElem
    array(k+1:old_size+1) = temp(k:old_size)

    deallocate(temp)
  end subroutine arrayInsert_int


  subroutine arrayInsert_intRange(array, k, newElems)
    implicit none
    integer, allocatable, intent(inout) :: array(:)
    integer, intent(in) :: newElems(:)
    integer, intent(in) :: k

    integer, allocatable :: temp(:)
    integer :: old_size
    integer :: added
    integer :: error

    ! Move contents of `array` into `temp`
    old_size = size(array)
    added = size(newElems)
    call move_alloc(array, temp)

    if (k < 1) then
      print *, "STOP", k
      return
    end if

    ! Add newElems to array
    allocate(array(old_size + added), stat=error)
    call alloc_check(error)
    array(1:k-1) = temp(1:k-1)
    array(k:k+added-1) = newElems(1:added)
    array(k+added:old_size+added) = temp(k:old_size)

    deallocate(temp)
  end subroutine arrayInsert_intRange


  subroutine arrayInsert_real(array, k, newElem)
    implicit none
    real(kind=real64), allocatable, intent(inout):: array(:)
    real(kind=real64), intent(in) :: newElem
    integer, intent(in) :: k

    real(kind=real64), allocatable  :: temp(:)
    integer :: old_size
    integer :: error

    old_size = size(array)
    call move_alloc(array, temp)

    if (k < 1) then
      print *, "STOP", k
      return
    end if

    allocate(array(old_size + 1), stat=error)
    call alloc_check(error)
    array(1:k-1) = temp(1:k-1)
    array(k) = newElem
    array(k+1:old_size+1) = temp(k:old_size)

    deallocate(temp)
  end subroutine arrayInsert_real


  subroutine arrayInsert_realRange(array, k, newElems)
    implicit none
    real(kind=real64), allocatable, intent(inout) :: array(:)
    real(kind=real64), intent(in) :: newElems(:)
    integer, intent(in) :: k

    real(kind=real64), allocatable :: temp(:)
    integer :: old_size
    integer :: added
    integer :: error

    ! Move contents of `array` into `temp`
    old_size = size(array)
    added = size(newElems)
    call move_alloc(array, temp)

    if (k < 1) then
      print *, "STOP", k
      return
    end if

    ! Add newElems to array
    allocate(array(old_size + added), stat=error)
    call alloc_check(error)
    array(1:k-1) = temp(1:k-1)
    array(k:k+added-1) = newElems(1:added)
    array(k+added:old_size+added) = temp(k:old_size)

    deallocate(temp)
  end subroutine arrayInsert_realRange

  ! === ARRAY REMOVE ===
  subroutine arrayRemove_int(array,  k)
    implicit none
    integer, allocatable, intent(inout) :: array(:)
    integer, intent(in) :: k

    integer, allocatable :: temp(:)
    integer :: old_size
    integer :: error

    ! Move contents of `array` to a temporary array.
    old_size = size(array)
    call move_alloc(array, temp)

    if (k > old_size .or. k < 1) then
      print "(a, i4, a, i4)", "STOP", k, ">", old_size
      return
    end if

    ! Reassign elements of `array` w/o the kth element.
    allocate(array(old_size - 1), stat=error)
    call alloc_check(error)
    array(1:k-1) = temp(1:k-1)
    array(k:old_size-1) = temp(k+1:size(temp))
    
    deallocate(temp)
  end subroutine arrayRemove_int


  subroutine arrayRemove_real(array,  k)
    implicit none
    real(kind=real64), allocatable, intent(inout) :: array(:)
    integer, intent(in) :: k

    real(kind=real64), allocatable :: temp(:)
    integer :: old_size
    integer :: error

    ! Move contents of `array` to a temporary array.
    old_size = size(array)
    call move_alloc(array, temp)

    if (k > old_size .or. k < 1) then
      print "(a, i4, a, i4)", "STOP", k, ">", old_size
      return
    end if

    ! Reassign elements of `array` w/o the kth element.
    allocate(array(old_size - 1), stat=error)
    call alloc_check(error)
    array(1:k-1) = temp(1:k-1)
    array(k:old_size-1) = temp(k+1:size(temp))
    
    deallocate(temp)
  end subroutine arrayRemove_real

  ! === ARRAY REMOVE RANGE SPECIFIC PROCEDURES ===
  subroutine arrayRemoveRange_int(array, a, b)
    implicit none
    integer, allocatable, intent(inout) :: array(:)
    integer, intent(in) :: a
    integer, intent(in) :: b

    integer, allocatable :: temp(:)
    integer :: old_size
    integer :: new_size
    integer :: error

    if (a > b .or. a < 1) then
      print "(a, i3, i3)", "Improbable range: ", a, b 
      return
    end if

    if (a > size(array) .or. b > size(array)) then
      print *, "STOP", a, b
      return
    end if

    ! Move contents of `array` into a temporary array.
    old_size = size(array)
    new_size = old_size - (b - a) - 1
    call move_alloc (array, temp)

    ! Reassign elements to `array` w/o the elements of index `a` to `b`
    allocate (array(new_size), stat=error)
    call alloc_check(error)
    array(1:a-1) = temp(1:a-1)
    array(a:new_size) = temp(b+1:size(temp))

    deallocate(temp)
  end subroutine arrayRemoveRange_int


  subroutine arrayRemoveRange_real(array, a, b)
    implicit none
    real(kind=real64), allocatable, intent(inout) :: array(:)
    integer, intent(in) :: a
    integer, intent(in) :: b

    real(kind=real64), allocatable :: temp(:)
    integer :: old_size
    integer :: new_size
    integer :: error

    if (a > b .or. a < 1) then
      print "(a, i3, i3)", "Improbable range: ", a, b 
      return
    end if

    if (a > size(array) .or. b > size(array)) then
      print *, "STOP", a, b
      return
    end if

    ! Move contents of `array` into a temporary array.
    old_size = size(array)
    new_size = old_size - (b - a) - 1
    call move_alloc (array, temp)

    ! Reassign elements to `array` w/o the elements of index `a` to `b`
    allocate (array(new_size), stat=error)
    call alloc_check(error)
    array(1:a-1) = temp(1:a-1)
    array(a:new_size) = temp(b+1:size(temp))

    deallocate(temp)
  end subroutine arrayRemoveRange_real

  ! === ARRAY REMOVE ELEMENT SPECIFIC PROCEDURES ===
  subroutine arrayRemoveElem_int(array, elem)
    implicit none
    integer, allocatable, intent(inout) :: array(:)
    integer, intent(in) :: elem
    integer :: i

    ! Do linear search. NOTE: Apparently `findloc` is not supported by
    ! gfortran 8.1.0
    do i = 1, size(array)
      if (array(i) == elem) then
        call arrayRemove_int(array, i)
        return
      end if
    end do
  end subroutine arrayRemoveElem_int


  subroutine arrayRemoveElem_real(array, elem)
    implicit none
    real(kind=real64), allocatable, intent(inout) :: array(:)
    real(kind=real64), intent(in) :: elem
    integer :: i

    ! Do linear search. NOTE: Apparently `findloc` is not supported by
    ! gfortran 8.1.0
    do i = 1, size(array)
      if (array(i) == elem) then
        call arrayRemove_real(array, i)
        return
      end if
    end do
  end subroutine arrayRemoveElem_real


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: alloc_check
  !>  Check whether the statements `allocate`, `deallocate` or `move_alloc` are
  !   run successfully. If they fail, the program is stopped. Otherwise, do
  !   nothing.
  ! -------------------------------------------------------------------------- !
  subroutine alloc_check(x)
    implicit none
    integer, intent(in) :: x
    if (x /= 0) then
      print "(a, i2)", "ERROR: Space cannot be allocated. status value: ", x
      stop
    end if
  end subroutine alloc_check


  ! NOTE: DEPRECATED
  subroutine extend_logical_array(array, elem, n)
    implicit none
    logical, allocatable, intent(inout) :: array(:)
    logical, intent(in) :: elem
    integer, intent(in) :: n

    logical :: newElems(n)
    logical, allocatable :: temp(:)
    integer :: i, old_size

    if (n < 1) then
        return
    end if

    newElems = [(elem, i = 1, n)]

    old_size = size(array)
    call move_alloc(array, temp)
    allocate(array(old_size + n))
    array(1:old_size) = temp(:)
    array(old_size+1:n) = newElems(:)

    deallocate(temp)
  end subroutine extend_logical_array
end module UpdateArray

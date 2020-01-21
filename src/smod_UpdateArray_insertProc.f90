submodule (UpdateArray) InsertProcedure
  implicit none
contains


  subroutine arrayInsert_int(array, k, newElem)
    integer(kind=arrIK), allocatable, intent(inout):: array(:)
    integer(kind=arrIK),              intent(in)   :: newElem
    integer,                          intent(in)   :: k

    integer(kind=arrIK), allocatable  :: temp(:)
    integer :: old_size
    integer :: error

    old_size = size(array)
    call move_alloc(array, temp)

    if (k < 1) then
      print "(a, i0, a)", "***ERROR. ", k, " is not a valid index."
      error stop
    end if

    allocate(array(old_size + 1), stat=error)
    call allocCheck(error)
    array(1:k-1) = temp(1:k-1)
    array(k) = newElem
    array(k+1:old_size+1) = temp(k:old_size)

    deallocate(temp)
  end subroutine arrayInsert_int


  ! === ARRAY INSERT SPECIFIC PROCEDURE===
  subroutine arrayInsert_intRange(array, k, newElems)
    integer(kind=arrIK), allocatable, intent(inout) :: array(:)
    integer(kind=arrIK),              intent(in)    :: newElems(:)
    integer,                          intent(in)    :: k

    integer(kind=arrIK), allocatable :: temp(:)
    integer :: old_size
    integer :: added
    integer :: error

    ! Move contents of `array` into `temp`
    old_size = size(array)
    added = size(newElems)
    call move_alloc(array, temp)

    if (k < 1) then
      print "(a, i0, a)", "***ERROR. ", k, " is not a valid index."
      error stop
    end if

    ! Add newElems to array
    allocate(array(old_size + added), stat=error)
    call allocCheck(error)
    array(1:k-1) = temp(1:k-1)
    array(k:k+added-1) = newElems(1:added)
    array(k+added:old_size+added) = temp(k:old_size)

    deallocate(temp)
  end subroutine arrayInsert_intRange


  ! === ARRAY INSERT SPECIFIC PROCEDURE===
  subroutine arrayInsert_real(array, k, newElem)
    real(kind=arrRK), allocatable, intent(inout):: array(:)
    real(kind=arrRK),              intent(in)   :: newElem
    integer,                       intent(in)   :: k

    real(kind=arrRK), allocatable  :: temp(:)
    integer :: old_size
    integer :: error

    old_size = size(array)
    call move_alloc(array, temp)

    if (k < 1) then
      print "(a, i0, a)", "***ERROR. ", k, " is not a valid index."
      error stop
    end if

    allocate(array(old_size + 1), stat=error)
    call allocCheck(error)
    array(1:k-1) = temp(1:k-1)
    array(k) = newElem
    array(k+1:old_size+1) = temp(k:old_size)

    deallocate(temp)
  end subroutine arrayInsert_real


  subroutine arrayInsert_realRange(array, k, newElems)
    real(kind=arrRK), allocatable, intent(inout) :: array(:)
    real(kind=arrRK),              intent(in)    :: newElems(:)
    integer,                       intent(in)    :: k

    real(kind=arrRK), allocatable :: temp(:)
    integer :: old_size
    integer :: added
    integer :: error

    ! Move contents of `array` into `temp`
    old_size = size(array)
    added = size(newElems)
    call move_alloc(array, temp)

    if (k < 1) then
      print "(a, i0, a)", "***ERROR. ", k, " is not a valid index."
      error stop
    end if

    ! Add newElems to array
    allocate(array(old_size + added), stat=error)
    call allocCheck(error)
    array(1:k-1) = temp(1:k-1)
    array(k:k+added-1) = newElems(1:added)
    array(k+added:old_size+added) = temp(k:old_size)

    deallocate(temp)
  end subroutine arrayInsert_realRange
end submodule

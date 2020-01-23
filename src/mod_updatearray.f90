module UpdateArray
  ! -------------------------------------------------------------------------- !
  ! MODULE: UpdateArray
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for array manipulations which involve
  !   allocations and deallocations.
  ! -------------------------------------------------------------------------- !
  use iso_fortran_env, only: int32, real32
  implicit none
  private

  ! Note: Can be changed when this module is to be reused in other projects.
  integer, public, parameter :: arrIK = int32
    !! Integer kind for integer arrays to be modified in this module.
  integer, public, parameter :: arrRK = real32
    !! Real kind for real arrays to be modified in this module.

  ! SUBMODULE INTERFACES
  !----------------------------------------------------------------------------!
  ! Insert procedures.
  interface
    module subroutine arrayInsert_int(array, k, newElem)
      integer(kind=arrIK), allocatable, intent(inout):: array(:)
      integer(kind=arrIK),              intent(in)   :: newElem
      integer,                          intent(in)   :: k
    end subroutine

    module subroutine arrayInsert_intRange(array, k, newElems)
      integer(kind=arrIK), allocatable, intent(inout) :: array(:)
      integer(kind=arrIK),              intent(in)    :: newElems(:)
      integer,                          intent(in)    :: k
    end subroutine

    module subroutine arrayInsert_real(array, k, newElem)
      real(kind=arrRK), allocatable, intent(inout):: array(:)
      real(kind=arrRK),              intent(in)   :: newElem
      integer,                       intent(in)   :: k
    end subroutine

    module subroutine arrayInsert_realRange(array, k, newElems)
      real(kind=arrRK), allocatable, intent(inout) :: array(:)
      real(kind=arrRK),              intent(in)    :: newElems(:)
      integer,                       intent(in)    :: k
    end subroutine  
  end interface

  ! Remove procedures.
  interface
    module subroutine arrayRemove_int(array,  k)
      integer(kind=arrIK), allocatable, intent(inout) :: array(:)
      integer,                          intent(in)    :: k
    end subroutine

    module subroutine arrayRemove_real(array,  k)
      real(kind=arrRK), allocatable, intent(inout) :: array(:)
      integer,                       intent(in)    :: k
    end subroutine

    module subroutine arrayRemoveRange_int(array, a, b)
      integer(kind=arrIK), allocatable, intent(inout) :: array(:)
      integer(kind=arrIK),              intent(in)    :: a
      integer,                          intent(in)    :: b
    end subroutine

    module subroutine arrayRemoveRange_real(array, a, b)
      real(kind=arrRK), allocatable, intent(inout) :: array(:)
      integer,                       intent(in)    :: a
      integer,                       intent(in)    :: b
    end subroutine

    module subroutine arrayRemoveElem_int(array, elem)
      integer(kind=arrIK), allocatable, intent(inout) :: array(:)
      integer(kind=arrIK),              intent(in)    :: elem
    end subroutine

    module subroutine arrayRemoveElem_real(array, elem)
      real(kind=arrRK), allocatable, intent(inout) :: array(:)
      real(kind=arrRK),              intent(in)    :: elem
    end subroutine
  end interface

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
  end interface

  ! -------------------------------------------------------------------------- !
  ! GENERIC SUBROUTINE: generateIndices
  !>  Remove the `k`th element of an array.
  ! -------------------------------------------------------------------------- !
  interface arrayRemove
    procedure :: arrayRemove_int
    procedure :: arrayRemove_real
  end interface

  ! -------------------------------------------------------------------------- !
  ! GENERIC SUBROUTINE: arrayRemoveRange
  !>  Remove a range of elements from an array.
  ! -------------------------------------------------------------------------- !
  interface arrayRemoveRange
    procedure :: arrayRemoveRange_int
    procedure :: arrayRemoveRange_real
  end interface

  ! -------------------------------------------------------------------------- !
  ! GENERIC SUBROUTINE: arrayRemoveElem
  !>  Remove the specified element from an array. If the element is not found,
  !   the subroutine passes silently.
  ! -------------------------------------------------------------------------- !
  interface arrayRemoveElem
    procedure :: arrayRemoveElem_int
    procedure :: arrayRemoveElem_real
  end interface
  !----------------------------------------------------------------------------!

  public :: arrayInsert
  public :: arrayRemove
  public :: arrayRemoveRange
  public :: arrayRemoveElem
  public :: allocCheck
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: allocCheck
  !>  Check whether the statements `allocate`, `deallocate` or `move_alloc` are
  !   run successfully. If they fail, the program is stopped. Otherwise, do
  !   nothing.
  ! -------------------------------------------------------------------------- !
  subroutine allocCheck(x)
    integer, intent(in) :: x
      !! Status of invoked `allocate` statement.

    if (x /= 0) then
      print "(a, i0)", "***ERROR: Space cannot be allocated. status value: ", x
      error stop
    end if
  end subroutine allocCheck
end module UpdateArray

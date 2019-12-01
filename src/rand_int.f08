module RandInd
  !----------------------------------------------------------------------------!
  ! MODULE:  Module name
  !----------------------------------------------------------------------------!
  ! DESCRIPTION: 
  !>  Module containing procedures for generating array of random
  !!  integers.
  !----------------------------------------------------------------------------!
  use StdKind, only: personIntKind, personRealKind
  implicit none
  private

  ! Module integer and real kinds.
  ! Note: Can be changed when this model is to be reused in other projects. 
  integer, parameter :: realKind = personRealKind
  integer, parameter :: intKind = personIntKind

  public :: generateIndices
  public :: generateRandInt
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: generateIndices
  !>  Generate a rank-1 array of unique integers in the interval
  !!  [lower, upper]. The length of the generated array is that of
  !!  the passed `indices`.
  ! -------------------------------------------------------------------------- !
  subroutine generateIndices(lower, upper, indices)
    implicit none

    integer(kind=intKind), intent(in) :: upper
    integer(kind=intKind), intent(in) :: lower
    integer(kind=intKind), intent(out) :: indices(:)
    
    integer(kind=intKind) :: index = 0
    real(kind=realKind)   :: random = 0
    integer :: L
    integer :: i

    indices(:) = 0
    L = size(indices)
    do i = 1, L
      do
        call random_number(random)
        index = floor(random*(upper - lower + 1)) + lower

        if (all(indices(1:i) /= index) .or. L > (upper - lower + 1)) then
          indices(i) = index
          exit
        end if

      end do
    end do
  end subroutine generateIndices


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: generateRandInt
  !>  Generate a random integer in the interval [a, b].
  ! -------------------------------------------------------------------------- !
  function generateRandInt(a, b) result(out)
    implicit none

    integer(kind=intKind), intent(in) :: a
    integer(kind=intKind), intent(in) :: b

    integer(kind=intKind) :: out
    real(kind=realKind)   :: random
  
    call random_number(random)
    out = floor(random*(b - a + 1)) + a
  end function generateRandInt
end module RandInd

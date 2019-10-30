module RandInd
  !----------------------------------------------------------------------------!
  ! MODULE:  Module name
  !----------------------------------------------------------------------------!
  ! DESCRIPTION: 
  !>  Module containing procedures for generating array of random integers.
  !----------------------------------------------------------------------------!
  use PersonType, only: stdIntKind, stdRealKind
  implicit none
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: generateIndices
  !>  Generate a rank-1 array of unique integers in the interval [lower, upper].
  !   The length of the generated array is that of the passed `indices`.
  ! -------------------------------------------------------------------------- !
  subroutine generateIndices(lower, upper, indices)
    implicit none
    integer(kind=stdIntKind), intent(in) :: upper
    integer(kind=stdIntKind), intent(in) :: lower
    integer(kind=stdIntKind), intent(out) :: indices(:)
    
    integer(kind=stdIntKind) :: index
    real(kind=stdRealKind) :: random
    integer :: i
    integer :: L

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
    integer(kind=stdIntKind), intent(in) :: a
    integer(kind=stdIntKind), intent(in) :: b
    integer(kind=stdIntKind) :: out

    real(kind=stdRealKind) :: random
  
    call random_number(random)
    out = floor(random*(b - a + 1)) + a
  end function generateRandInt
end module RandInd

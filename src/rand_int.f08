module RandInd
  !----------------------------------------------------------------------------!
  ! MODULE:  Module name
  !----------------------------------------------------------------------------!
  ! DESCRIPTION: 
  !>  Module containing procedures for generating array of random
  !!  integers.
  !----------------------------------------------------------------------------!
  use PersonType, only: personIK, personRK
  implicit none
  private

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

    integer(kind=personIK), intent(in) :: upper
    integer(kind=personIK), intent(in) :: lower
    integer(kind=personIK), intent(out) :: indices(:)
    
    integer(kind=personIK) :: index = 0
    real(kind=personRK)    :: random = 0
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

    integer(kind=personIK), intent(in) :: a
    integer(kind=personIK), intent(in) :: b

    integer(kind=personIK) :: out
    real(kind=personRK)    :: random
  
    call random_number(random)
    out = floor(random*(b - a + 1)) + a
  end function generateRandInt
end module RandInd

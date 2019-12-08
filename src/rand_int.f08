module RandInd
  !----------------------------------------------------------------------------!
  ! MODULE:  Module name
  !----------------------------------------------------------------------------!
  ! DESCRIPTION: 
  !>  Module containing procedures for generating array of random
  !!  integers.
  !----------------------------------------------------------------------------!
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

    integer, intent(in)  :: upper
    integer, intent(in)  :: lower
    integer, intent(out) :: indices(:)
    
    integer :: index = 0
    real    :: random = 0
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

    integer, intent(in) :: a
    integer, intent(in) :: b

    integer :: out
    real    :: random
  
    call random_number(random)
    out = floor(random*(b - a + 1)) + a
  end function generateRandInt
end module RandInd

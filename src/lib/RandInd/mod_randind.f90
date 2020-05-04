module RandInd
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Module name
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing procedures for generating array of random
  !!  integers.
  ! -------------------------------------------------------------------------- !
  use RNG, only: getRandNumber
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
    integer, intent(in)  :: upper
      !! The inclusive upper bound of integers to be randomly generated.
    integer, intent(in)  :: lower
      !! The inclusive lower bound of integers to be randomly generated.
    integer, intent(out) :: indices(:)
      !! The array of random integers to be used as randomized indices.
    
    integer :: index = 0
    real    :: random = 0
    integer :: i

    indices(:) = lower - 1
    do i = 1, size(indices)
      do
        call getRandNumber(random)
        index = floor(random*(upper - lower + 1)) + lower

        if (all(indices(1:i) /= index)) then
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
    integer, intent(in) :: a
      !! The inclusive upper bound of the integer to be randomly generated.
    integer, intent(in) :: b
      !! The inclusive lower bound of the integer to be randomly generated.

    integer :: out
    real    :: random
  
    call getRandNumber(random)
    out = floor(random*(b - a + 1)) + a
  end function generateRandInt
end module RandInd

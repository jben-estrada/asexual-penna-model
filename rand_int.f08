module RandInd
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  ! Module containing procedures for generating unique set of integers.
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  use PersonType, only: stdIntKind, stdRealKind
  implicit none
contains

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

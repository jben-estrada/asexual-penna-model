module TickerType
  implicit none
  private

  type, public :: Ticker
    integer :: partition
    integer :: totalTicks
    integer :: index
    character :: charBit
  contains
    procedure :: showTicker
    procedure :: incrementTick
  end type

  character, parameter :: defaultCharBit = ">"
  public :: constructTicker
contains

  function constructTicker(partition, totalTicks, charBit) result(new)
    implicit none
    integer, intent(in) :: partition
    integer, intent(in) :: totalTicks
    character, optional :: charBit
    type(Ticker) :: new

    if (present(charBit)) then
      new%charBit = charBit
    else
      new%charBit = defaultCharBit
    end if

    new%index = 0
    new%partition = partition
    new%totalTicks = totalTicks
  end function constructTicker


  subroutine incrementTick(self, show, increment)
    implicit none
    class(Ticker), intent(inout) :: self
    logical, optional :: show
    integer, optional :: increment

    integer :: increment_
    logical :: show_

    if (self%index == self%totalTicks) return

    if (present(increment)) then
      increment_ = increment
      self%index = self%index + increment_
    else
      self%index = self%index + 1
    end if

    if (present(show)) then
      show_ = show
      if (show_) then
        call self%showTicker
      end if
    end if
  end subroutine incrementTick


  subroutine showTicker(self)
    implicit none
    class(Ticker), intent(inout) :: self

    character, allocatable :: tickArr(:)
    integer :: tickerLength
    integer :: i

    tickerLength = int(self%index*self%partition/self%totalTicks)
    tickArr = [(self%charBit, i = 1, tickerLength), &
        (" ", i = self%partition - 1, tickerLength, -1)]

    write(*, "(*(a))", advance="no") (char(8), i = 1, self%partition + 7)
    write(*, "(*(a))", advance="no") "[", tickArr, "]"
    write(*, "(i4, a)", advance="no") tickerLength*100/self%partition, "%"

    deallocate(tickArr)
  end subroutine showTicker
end module TickerType

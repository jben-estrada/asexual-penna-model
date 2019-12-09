module TickerType
  !----------------------------------------------------------------------------!
  ! MODULE:  TickerType
  !----------------------------------------------------------------------------!
  ! DESCRIPTION: 
  !>  Module containing a Ticker derived type for displaying "ticker"
  !!  or progress bar.
  !----------------------------------------------------------------------------!
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


  !----------------------------------------------------------------------------!
  ! SUBROUTINE: constructTicker
  !>  A constructor procedure for the `Ticker` type.
  !----------------------------------------------------------------------------!
  function constructTicker(partition, totalTicks, charBit) result(new)
    implicit none

    integer, intent(in) :: partition
    integer, intent(in) :: totalTicks
    character, optional :: charBit
    type(Ticker)        :: new

    ! Get character bit for progress bar.
    if (present(charBit)) then
      new%charBit = charBit
    else
      new%charBit = defaultCharBit
    end if

    ! Initialize `Ticker` object.
    new%index = 0
    new%partition = partition
    new%totalTicks = totalTicks
  end function constructTicker


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Ticker%]incrementTick
  !>  Increment `index` attribute of a `Ticker` type. The increment
  !!  value can be optionally changed. It can optionally show the
  !!  ticker progress as well.
  !----------------------------------------------------------------------------!
  subroutine incrementTick(self, show, increment)
    implicit none
    class(Ticker), intent(inout) :: self

    logical, optional :: show
    integer, optional :: increment

    ! End of progress bar case.
    if (self%index == self%totalTicks) return

    ! Increment internal index for progress bar.
    if (present(increment)) then
      self%index = self%index + increment
    else
      self%index = self%index + 1
    end if

    ! Optionally show the progress bar.
    if (present(show)) then
      if (show) then
        call self%showTicker
      end if
    end if
  end subroutine incrementTick


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Ticker%]showTicker
  !>  Show the ticker progres.
  !----------------------------------------------------------------------------!
  subroutine showTicker(self)
    implicit none
    class(Ticker), intent(inout) :: self

    character, allocatable :: tickArr(:)
    integer :: tickerLength
    integer :: i

    tickerLength = int(self%index*self%partition/self%totalTicks)

    ! Get progress bar as a character array.
    allocate(tickArr(tickerLength))
    if (tickerLength > 0) then
      tickArr = [(self%charBit, i = 1, tickerLength), &
          (" ", i = self%partition - 1, tickerLength, -1)]
    else
      tickArr = [(" ", i = self%partition - 1, tickerLength, -1)]
    end if

    ! Print character array.
    write(*, "(*(a))", advance="no") (char(8), i = 1, self%partition)
    write(*, "(*(a))", advance="no") "[", tickArr, "]"

    deallocate(tickArr)
  end subroutine showTicker
end module TickerType

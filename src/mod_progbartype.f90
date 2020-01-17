module ProgBarType
  !----------------------------------------------------------------------------!
  ! MODULE:  ProgBarType
  !----------------------------------------------------------------------------!
  ! DESCRIPTION: 
  !>  Module containing a derived type for printing progress bars.
  !----------------------------------------------------------------------------!
  implicit none
  private

  type, public :: ProgressBar
    integer   :: partition
    integer   :: totalTicks
    integer   :: counter
    character :: charBit
  contains
    procedure :: showProgBar
    procedure :: incrementCounter
  end type ProgressBar

  character, parameter :: DEFAULT_CHAR_BIT = ">"
  public :: initProgressBar
contains


  !----------------------------------------------------------------------------!
  ! SUBROUTINE: initProgressBar
  !>  Initialize a `ProgressBar` object.
  !----------------------------------------------------------------------------!
  subroutine initProgressBar(new, partition, totalTicks, charBit)
    implicit none

    type(ProgressBar), intent(out) :: new
    integer,           intent(in)  :: partition
    integer,           intent(in)  :: totalTicks
    character,         optional    :: charBit

    ! Get character bit for progress bar.
    if (present(charBit)) then
      new % charBit = charBit
    else
      new % charBit = DEFAULT_CHAR_BIT
    end if

    ! Initialize `ProgressBar` object.
    new % counter = 0
    new % partition = partition
    new % totalTicks = totalTicks
  end subroutine initProgressBar


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [ProgressBar % ]incrementTick
  !>  Increment `counter` attribute of a `ProgressBar` object. The increment
  !!  value can be optionally changed. It can optionally show the
  !!  progress bar as well.
  !----------------------------------------------------------------------------!
  subroutine incrementCounter(self, increment, show)
    implicit none
    class(ProgressBar), intent(inout) :: self

    integer, optional :: increment
    logical, optional :: show

    ! End of progress bar case.
    if (self % counter == self % totalTicks) return

    ! Increment internal counter for progress bar.
    if (present(increment)) then
      self % counter = self % counter + increment
    else
      self % counter = self % counter + 1
    end if

    ! Optionally show the progress bar.
    if (present(show)) then
      if (show) then
        call self % showProgBar
      end if
    end if
  end subroutine incrementCounter


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [ProgressBar % ]showProgBar
  !>  Print the progress bar.
  !----------------------------------------------------------------------------!
  subroutine showProgBar(self)
    implicit none
    class(ProgressBar), intent(in) :: self

    character, allocatable :: tickArr(:)
    integer :: barLength
    integer :: i

    barLength = int(self % counter * self % partition/self % totalTicks)

    ! Get progress bar as a character array.
    allocate(tickArr(barLength))

    if (barLength > 0) then
      tickArr = [(self % charBit, i = 1, barLength), &
          (" ", i = self % partition - 1, barLength, -1)]
    else
      tickArr = [(" ", i = self % partition - 1, barLength, -1)]
    end if

    ! Print the character array.
    write(*, "(*(a))", advance="no") (char(8), i = 1, self % partition + 10)
    write(*, "(*(a))", advance="no") "[", tickArr, "]"
    write(*, "(f6.1, a)", advance="no") 100*real(self % counter) / &
        real(self % totalTicks), " % "

    deallocate(tickArr)
  end subroutine showProgBar
end module ProgBarType

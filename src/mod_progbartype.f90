module ProgBarType
  ! -------------------------------------------------------------------------- !
  ! MODULE:  ProgBarType
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing a derived type for printing progress bars.
  ! -------------------------------------------------------------------------- !
  implicit none
  private

  type, public :: ProgressBar
    !! A derived type for displaying progress bars.
    private
    integer   :: partition  !! Number at which `counter` is partitioned.
    integer   :: totalTicks !! Number to reach 100% of the progress bar.
    integer   :: counter    !! Number representing progress.
    character :: charBit    !! Character to be displayed to denote progress.
  contains
    procedure :: showProgBar
    procedure :: incrementCounter
  end type ProgressBar

  character, parameter :: DEFAULT_CHAR_BIT = ">"
    !! Default character bit.

  public :: initProgressBar
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initProgressBar
  !>  Initialize a `ProgressBar` object.
  ! -------------------------------------------------------------------------- !
  subroutine initProgressBar(new, partition, totalTicks, charBit)
    type(ProgressBar), intent(out) :: new
      !! `ProgressBar` object to be initialized.
    integer,           intent(in)  :: partition
      !! The `partition` for the `partition` attribute of `new`. 
    integer,           intent(in)  :: totalTicks
      !! The `totalTicks` for the `totalTicks` attribute of `new`. 
    character,         optional    :: charBit
      !! The `charBit` for the `charBit` attribute of `new`.
      !! Defaults to `DEFAULT_CHAR_BIT`.

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


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [ProgressBar % ]incrementTick
  !>  Increment `counter` attribute of a `ProgressBar` object. The increment
  !!  value can be optionally changed. It can optionally show the
  !!  progress bar as well.
  ! -------------------------------------------------------------------------- !
  subroutine incrementCounter(self, increment, show)
    class(ProgressBar), intent(inout) :: self
      !! `ProgressBar` object to be updated.
    integer, optional :: increment
      !! Number to increment the `counter` attribute of `self`.
    logical, optional :: show
      !! Show the progress bar if true. Deafults to `.false.`

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


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [ProgressBar % ]showProgBar
  !>  Print the progress bar.
  ! -------------------------------------------------------------------------- !
  subroutine showProgBar(self)
    class(ProgressBar), intent(in) :: self
      !! `ProgressBar` object to be shown.

    character, allocatable :: tickArr(:)
    integer :: barLength
    integer :: i

    barLength = int(self % counter &
        * real(self % partition)/real(self % totalTicks))

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
        real(self % totalTicks), "%"

    deallocate(tickArr)
  end subroutine showProgBar
end module ProgBarType

module ProgressBarType
  ! -------------------------------------------------------------------------- !
  ! MODULE:  ProgressBarType
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing a derived type for printing progress bars.
  ! -------------------------------------------------------------------------- !
  use ErrorMSG, only: raiseError
  use CastProcs, only: castIntToChar
  implicit none
  private

  type :: ProgressBar_t
    !! A derived type for displaying progress bars.
    private
    integer   :: partition   !! Number at which `counter` is partitioned.
    integer   :: totalTicks  !! Number to reach 100% of the progress bar.
    integer   :: counter     !! Number representing progress.
    character :: charBit     !! Character to be displayed to denote progress.
    integer   :: totalBarLen !! Required output length of the progress bar.
  contains
    procedure :: showProgBar => progressbar_showProgBar
      !! Print the progress bar.
    procedure :: incrCounter => progressbar_incrCounter
      !! Increment the progress counter. The increment value, which defaults to
      !! 1, can be optionally changed.
    procedure :: clear => progressbar_clear
      !! Clear the progress bar.
  end type ProgressBar_t

  character, parameter :: DEFAULT_CHAR_BIT = ">"
    !! Default character bit.

  integer, parameter :: BRACKET_LEN = 2 ! Left and right bracket 
  integer, parameter :: SPACING_LEN = 1 ! Spacing between percent and bar proper
  integer, parameter :: PERCENT_LEN = 6 ! = 5 (percent str) + 1 ("%" char)

  character(len=*), parameter :: PERCENT_OUT_FMT = "f5.1"

  public :: ProgressBar_t
  public :: init_ProgressBar
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: init_ProgressBar
  !>  Initializer for `ProgressBar_t` objects.
  ! -------------------------------------------------------------------------- !
  subroutine init_ProgressBar(new, totalTicks, charBit, partition, totalBarLen)
    type(ProgressBar_t), intent(inout) :: new
      !! `ProgressBar` object to be initialized.
    integer,             intent(in)    :: totalTicks
    !! The `totalTicks` for the `totalTicks` attribute of `new`. 
    character, optional, intent(in)    :: charBit
    !! The `charBit` for the `charBit` attribute of `new`.
    !! Defaults to `DEFAULT_CHAR_BIT`.
    integer,   optional, intent(in)    :: partition
      !! The `partition` for the `partition` attribute of `new`. 
    integer,   optional, intent(in)    :: totalBarLen
      !! The length of the entire progress bar.

    integer, parameter :: INVARIANT_STR_LEN = BRACKET_LEN + PERCENT_LEN

    ! Get character bit for progress bar.
    if (present(charBit)) then
      new % charBit = charBit
    else
      new % charBit = DEFAULT_CHAR_BIT
    end if

    if (present(totalBarLen) .and. present(partition)) then
      call raiseError("'totalBarLen' and 'partition' cannot be both present")
    end if

    ! Set the dimensions of the elements of the progress bar based on
    ! the total length of the bar.
    if (present(totalBarLen)) then
      new % totalBarLen = totalBarLen
      new % partition = totalBarLen - (INVARIANT_STR_LEN + SPACING_LEN)

      if (new % partition <= 0) then
        call raiseError(  &
          "Progress bar too short. " //                                       &
          "Total length must not be less than the invariant char length (" // &
          castIntToChar(INVARIANT_STR_LEN) // ")"                             &
        )
      end if
    end if

    ! Set the dimensions of the elements of the progress bar based on
    ! the number of partitions in the progress bar (the char bit that moves)
    if (present(partition)) then
      if (partition < 0) then
        call raiseError("Cannot have negative number of partitions")
      end if

      new % totalBarLen = partition + INVARIANT_STR_LEN
      new % partition = partition
    end if

    ! Initialize `ProgressBar` object.
    new % counter = 0
    new % totalTicks = totalTicks
  end subroutine init_ProgressBar


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: progressbar_incrCounter
  !>  Increment `counter` attribute of a `ProgressBar` object. The increment
  !!  value can be optionally changed. It can optionally show the
  !!  progress bar as well.
  ! -------------------------------------------------------------------------- !
  subroutine progressbar_incrCounter(self, increment, show)
    class(ProgressBar_t), intent(inout) :: self
      !! `ProgressBar` object to be updated.
    integer,              optional      :: increment
      !! Number to increment the `counter` attribute of `self`.
    logical,              optional      :: show
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
        call self % showProgBar()
      end if
    end if
  end subroutine progressbar_incrCounter


  ! -------------------------------------------------------------------------- !
  !  SUBROUTINE: progressbar_showProgBar
  !>  Print the progress bar.
  ! -------------------------------------------------------------------------- !
  subroutine progressbar_showProgBar(self)
    class(ProgressBar_t), intent(in) :: self
      !! `ProgressBar` object to be shown.

    character, allocatable :: tickArr(:)
    integer :: barLength, spacing
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

    spacing =   self % totalBarLen &
             - (self % partition + BRACKET_LEN + PERCENT_LEN)
    if (spacing < 0) spacing = SPACING_LEN

    ! Print the progress bar.
    write(*, "(*(a))", advance="no") "[", tickArr, "]", repeat(" ", spacing)
    write(*, "(" // trim(PERCENT_OUT_FMT) // ", a)", advance="yes")  &
        100 * real(self % counter) / real(self % totalTicks), "%"
    
    ! Move the cursor back to the beginning of the progress bar.
    ! NOTE: This uses ANSI escape codes. May not work on certain terminals.
    write(*, "(a)", advance="no") achar(27) // "[1A" 

    deallocate(tickArr)
  end subroutine progressbar_showProgBar


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: cursorToLeft
  !>  Clear the progress bar.
  ! -------------------------------------------------------------------------- !
  subroutine progressbar_clear(self)
    class(ProgressBar_t), intent(in) :: self
    ! Overwrite the place where the progress bar should be
    write(*, "(a)", advance="no") repeat(" ", self % totalBarLen)
    ! Move the cursor to the left
    write(*, "(a)", advance="no") achar(27) // "[1A"
  end subroutine progressbar_clear
end module ProgressBarType

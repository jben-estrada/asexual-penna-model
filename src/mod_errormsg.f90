module ErrorMSG
  ! -------------------------------------------------------------------------- !
  ! MODULE: ErrorMSG
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for printing error and warning messages.
  !!
  !!  Based from the `ansi_colors` module by Jason Blevins in
  !!  http://fortranwiki.org/fortran/show/ansi_colors
  ! -------------------------------------------------------------------------- !
  use ANSIEscCodes, only: formatChar, escCodeRed, escCodeYellow, escCodeBold
  implicit none
  private

  public :: raiseError
  public :: raiseWarning
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: raiseError
  !>  Print the error message `msg` and stop the program.
  ! -------------------------------------------------------------------------- !
  subroutine raiseError(msg)
    character(len=*), intent(in) :: msg
      !! Accompanying error message.
  
    ! Print the error message in red.
    print "(3a)", formatChar("***", escCodeRed), &
        formatChar("ERROR", escCodeRed // escCodeBold), &
        formatChar("*** " // msg, escCodeRed)
    error stop
  end subroutine raiseError


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: raiseWarning
  !>  Print the warning message `msg` and stop the program if one so chooses.
  ! -------------------------------------------------------------------------- !
  subroutine raiseWarning(msg, withWarningTxt, stopProgram)
    character(len=*),  intent(in) :: msg
      !! Accompanying warning message.
    logical, optional, intent(in) :: withWarningTxt
      !! Print "***WARNING***" when true.
    logical, optional, intent(in) :: stopProgram
      !! Stop the program with the `error stop` statement when true.

    ! Print "***WARNING***" to signify the warning.
    if (present(withWarningTxt)) then
      if (withWarningTxt) write(*, "(3a)", advance="no") &
          formatChar("***", escCodeYellow), &
          formatChar("WARNING", escCodeYellow // escCodeBold), &
          formatChar("*** ", escCodeYellow)
    end if

    ! Print the warning message in yellow.
    print "(a)", formatChar(msg, escCodeYellow)
  
    ! Stop the program.
    if (present(stopProgram)) then
      if (stopProgram) error stop
    end if
  end subroutine raiseWarning
end module ErrorMSG
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
  !>  Print the error message `msg` and stop the program if one so chooses.
  !!  By default, this subroutine stops the program when it is called.
  ! -------------------------------------------------------------------------- !
  subroutine raiseError(msg, stopProgram)
    character(len=*), intent(in) :: msg
      !! Accompanying error message.
    logical, optional, intent(in) :: stopProgram
      !! Stop the program with the `error stop` statement when true.
  
    ! Print the error message in red.
    print "(3a)", formatChar("***", escCodeRed), &
        formatChar("ERROR", escCodeRed // escCodeBold), &
        formatChar("*** " // msg, escCodeRed)

    ! Stop the program.
    if (present(stopProgram)) then
      if (stopProgram) error stop
    else
      error stop
    end if
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
      if (withWarningTxt) write(*, "(4a)", advance="no") &
          formatChar("***", escCodeYellow), &
          formatChar("WARNING", escCodeYellow // escCodeBold), &
          formatChar("*** ", escCodeYellow), &
          formatChar(msg, escCodeYellow)
    else      
      ! Print only the warning message.
      print "(a)", formatChar(msg, escCodeYellow)
    end if

    ! Stop the program.
    if (present(stopProgram)) then
      if (stopProgram) error stop
    end if
  end subroutine raiseWarning
end module ErrorMSG
module ASCIIProcedure
  ! -------------------------------------------------------------------------- !
  ! MODULE: ASCIIProcedure
  ! -------------------------------------------------------------------------- !
  ! Author: John Benedick Estrada
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing a utility procedures related to ASCII characters
  ! -------------------------------------------------------------------------- !
  implicit none
  private

  integer, parameter :: ASCII_CODE_LARGE_A = iachar("A")
  integer, parameter :: ASCII_CODE_LARGE_Z = iachar("Z")
  integer, parameter :: ASCII_CODE_SMALL_A = iachar("a")
  integer, parameter :: ASCII_CODE_SMALL_Z = iachar("z")
  integer, parameter :: ASCII_CODE_ZERO    = iachar("0")
  integer, parameter :: ASCII_CODE_NINE    = iachar("9")
  integer, parameter :: FLIP_CASE_BIT = 32

  public :: isWhiteSpace
  public :: isAlphabetic
  public :: isDigit
  public :: isAlphaNumeric
  public :: toLower
  public :: toUpper
  public :: swapCase
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isWhiteSpace
  !>  Returns TRUE if all characters in the input string are white spaces.
  !!  Otherwise, returns FALSE.
  ! -------------------------------------------------------------------------- !
  pure logical function isWhiteSpace(str)
    character(len=*), intent(in) :: str
      !! Character to be inspected.

    integer :: asciiCode, i

    isWhiteSpace = .true.
    do i = 1, len(str)
      asciiCode = iachar(str(i:i))
      isWhiteSpace = (8 <= asciiCode .and. asciiCode <= 13) .or. asciiCode == 32
      if (.not.isWhiteSpace) exit
    end do
  end function isWhiteSpace


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isAlphabetic
  !>  Returns TRUE if all characters in the input string are alphabetic.
  !!  Otherwise, returns FALSE.
  ! -------------------------------------------------------------------------- !
  pure logical function isAlphabetic(str)
    character(len=*), intent(in) :: str
    integer :: asciiCode, strLen, i

    isAlphabetic = .true.
    strLen = len(str)
    i = 1
    do while (isAlphabetic .and. i <= strLen)
      asciiCode = ior(iachar(str(i:i)), FLIP_CASE_BIT)   ! To lowercase.
      isAlphabetic = (      (ASCII_CODE_SMALL_A <= asciiCode)  &
                      .and. (asciiCode          <= ASCII_CODE_SMALL_Z))
      i = i + 1
    end do
  end function isAlphabetic


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isDigit
  !>  Returns TRUE if all characters in the input string are digits.
  !!  Otherwise, returns FALSE.
  ! -------------------------------------------------------------------------- !
  pure logical function isDigit(str)
    character(len=*), intent(in) :: str
    integer :: asciiCode, strLen, i

    isDigit = .true.
    strLen = len(str)
    i = 1
    do while (isDigit .and. i <= strLen)
      asciiCode = iachar(str(i:i))
      isDigit = (      (ASCII_CODE_ZERO <= asciiCode)  &
                 .and. (asciiCode     <= ASCII_CODE_NINE))
      i = i + 1
    end do
  end function isDigit


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isAlphaNumeric
  !>  Returns TRUE if all characters in the input string are either digits
  !!  or alphabetic characters. Otherwise, returns FALSE.
  ! -------------------------------------------------------------------------- !
  pure logical function isAlphaNumeric(str)
    character(len=*), intent(in) :: str
    character :: c
    integer   :: i

    isAlphaNumeric = .true.
    do i = 1, len(str)
      c = str(i:i)
      isAlphaNumeric = isAlphabetic(c) .or. isDigit(c)
      if (.not.isAlphaNumeric) exit
    end do
  end function isAlphaNumeric


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: toLower
  !>  Returns a copy of the input string whose alphabetic characters are
  !!  converted to lowercase.
  ! -------------------------------------------------------------------------- !
  pure function toLower(str) result(resStr)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: resStr
    character :: c
    integer   :: i

    do i = 1, len(str)
      c = str(i:i)
      if (isAlphabetic(c)) then
        ! To lowercase
        c = achar( ior(iachar(c), FLIP_CASE_BIT) )
      end if
      resStr(i:i) = c
    end do
  end function toLower


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: toUpper
  !>  Returns a copy of the input string whose alphabetic characters are
  !!  converted to uppercase.
  ! -------------------------------------------------------------------------- !
  pure function toUpper(str) result(resStr)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: resStr
    character :: c
    integer   :: i

    do i = 1, len(str)
      c = str(i:i)
      if (isAlphabetic(c)) then
        ! To uppercase
        c = achar( iachar(c) - FLIP_CASE_BIT )
      end if
      resStr(i:i) = c
    end do
  end function toUpper


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: swapCase
  !>  Returns a copy of the input string whose alphabetic characters have their
  !!  respective cases swapped, i.e. lower to uppercase, or upper to lowercase.
  ! -------------------------------------------------------------------------- !
  pure function swapCase(str) result(resStr)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: resStr
    character :: c
    integer   :: i

    do i = 1, len(str)
      c = str(i:i)
      if (isAlphabetic(c)) then
        ! Flip case
        c = achar( ieor(iachar(c), FLIP_CASE_BIT) )
      end if
      resStr(i:i) = c
    end do
  end function swapCase
end module ASCIIProcedure
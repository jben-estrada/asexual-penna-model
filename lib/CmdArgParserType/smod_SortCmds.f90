submodule (CmdArgParserType) SortCmds
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: SortCmds
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `CmdArgParserType` containing procedures for sorting
  !!  character arrays.
  !!
  !!  The character precedence are outlined as follows:
  !!    i.) Shorter characters always have greater precendence over longer ones.
  !!    ii.) For characters with equal lengths, the precendence is determined
  !!         by their ASCII code. Needless to say, the character array to be
  !!         sorted must only contain ASCII characters. As for letters with
  !!         differing cases, the comparison is case-insensitive (e.g. "a" is
  !!         equal to "A").
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: sortCharArr
  !>  Short character array using bubble sort.
  !!  Character precendece goes as follows:
  !!    i.) Shorter characters always have greater precendence over longer ones.
  !!    ii.) For characters with equal lengths, the precendence is determined
  !!         by their ASCII code. Needless to say, the character array to be
  !!         sorted must only contain ASCII characters. As for letters with
  !!         differing cases, the comparison is case-insensitive (e.g. "a" is
  !!         equal to "A").
  ! -------------------------------------------------------------------------- !
  module subroutine sortCharArr(charArr)
    character(len=*), intent(inout) :: charArr(:)
      !! Character array to be sorted in place.

    integer :: i, j

    do i = lbound(charArr, 1), ubound(charArr, 1)
      do j = lbound(charArr, 1), ubound(charArr, 1) - i
        if (isFirstGreater(charArr(j), charArr(j+1))) then
          call swapCharElem(charArr, j, j+1)
        end if
      end do
    end do
  end subroutine sortCharArr


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isFirstGreater
  !>  Determine which character has the greater precendence than the other.
  ! -------------------------------------------------------------------------- !
  logical function isFirstGreater(char1, char2)
    character(len=*), intent(in) :: char1
      !! Character to be compared with `char1`.
    character(len=*), intent(in) :: char2
      !! Character to be compared with `char2`.

    integer :: i, asciiCode1, asciiCode2

    ! Initialize output.
    isFirstGreater = .false.

    if (len_trim(char1) == len_trim(char2)) then
      do i = 1, len_trim(char1)
        asciiCode1 = asciiCodeNoCase(char1(i:i))
        asciiCode2 = asciiCodeNoCase(char2(i:i))
        if (asciiCode1 /= asciiCode2) then
          isFirstGreater = asciiCode1 > asciiCode2
          exit
        end if
      end do

    else
      isFirstGreater = len_trim(char1) > len_trim(char2)
    end if    
  end function isFirstGreater


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: asciiCodeNoCase
  !>  Get the ASCII code of `char`. All letters are all turned to upper case.
  ! -------------------------------------------------------------------------- !
  integer function asciiCodeNoCase(char)
    character, intent(in) :: char

    asciiCodeNoCase = iachar(char)
    
    ! Upper-case and lower-case letters are 32 characters apart.
    if (97 <= asciiCodeNoCase .and. asciiCodeNoCase <= 122) then
      asciiCodeNoCase = asciiCodeNoCase - 32
    end if
  end function asciiCodeNoCase


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: swapCharElem
  !>  Swap two elements whose indices are `a` and `b` in the character array
  !!  `charArr`.
  ! -------------------------------------------------------------------------- !
  subroutine swapCharElem(charArr, a, b)
    character(len=*), intent(inout) :: charArr(:)
    integer,          intent(in)    :: a
    integer,          intent(in)    :: b

    character(len=len(charArr)) :: temp

    temp = charArr(a)
    charArr(a) = charArr(b)
    charArr(b) = temp
  end subroutine swapCharElem
end submodule SortCmds
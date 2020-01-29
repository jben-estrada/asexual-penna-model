module CastProcedures
  ! -------------------------------------------------------------------------- !
  ! MODULE: CastProcedures
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for casting values from one data type 
  !   to another.
  ! -------------------------------------------------------------------------- !
  implicit none
  
  integer, private, parameter :: MAX_LEN = 64
    !! Maximum character length for x-to-char procedures.
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castCharToInt
  !>  Cast the input character `char` into an integer.
  ! -------------------------------------------------------------------------- !
  integer function castCharToInt(char)
    character(len=*), intent(in) :: char
      !! Character to cast to integer.

    integer :: status
    read(char, *, iostat=status) castCharToInt

    if (status /= 0) then
      print "(*(a))", "***ERROR. '", trim(adjustl(char)), "' is not numeric."
      stop
    end if
  end function castCharToInt


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castIntToChar
  !>  Cast the input integer `int` into a 64 long character.
  ! -------------------------------------------------------------------------- !
  character(len=MAX_LEN) function castIntToChar(int)
    integer, pointer, intent(in) :: int
      !! Integer to cast to character.

    integer :: status
    write(castIntToChar, *, iostat=status) int

    if (status /= 0) then
      print "(a, i0, a)", "***ERROR. '", int, "' cannot be casted to character."
      stop
    end if
  end function castIntToChar
end module CastProcedures

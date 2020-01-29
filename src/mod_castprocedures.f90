module CastProcedures
  implicit none
  
  integer, private, parameter :: MAX_LEN = 64
    !! Maximum character length for x-to-char procedures.
contains
    

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

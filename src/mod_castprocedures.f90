module CastProcedures
  ! -------------------------------------------------------------------------- !
  ! MODULE: CastProcedures
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for casting values from one data type 
  !!  to another.
  ! -------------------------------------------------------------------------- !
  use ErrorMSG, only: raiseError
  implicit none
  private
  
  integer, parameter :: MAX_LEN = 64
    !! Maximum character length for x-to-char procedures.

  ! Default read/write formats.
  character(len=*), parameter :: DEF_INT_FORMAT = "(i10)"
  character(len=*), parameter :: DEF_REAL_FORMAT = "(f10.5)"

  public :: castCharToInt
  public :: castIntToChar
  public :: castIntPtrToChar
  public :: castRealToChar
  public :: castCharToReal
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castCharToInt
  !>  Cast the input character `char` into an integer.
  ! -------------------------------------------------------------------------- !
  integer function castCharToInt(char, caststat)
    character(len=*), intent(in) :: char
      !! Character to cast to integer.
    integer, optional, intent(out) :: castStat
    !! Casting status.

    integer :: status
    read(char, DEF_INT_FORMAT, iostat=status) castCharToInt

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (status /= 0) &
        call raiseError("'" // trim(adjustl(char)) // "' is not numeric.")
    end if
  end function castCharToInt


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castIntToChar
  !>  Cast the integer `int` into a 64 long character.
  ! -------------------------------------------------------------------------- !
  character(len=MAX_LEN) function castIntToChar(int, castStat)
    integer,           intent(in) :: int
      !! Integer to cast to character.
    integer, optional, intent(out) :: castStat
      !! Casting status.

    integer :: status

    ! Initialize the output.
    castIntToChar = ""

    write(castIntToChar, DEF_INT_FORMAT, iostat=status) int

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (status /= 0) call raiseError("Casting from int to char failed.")
    end if
  end function castIntToChar


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castIntPtrToChar
  !>  Cast the integer pointer `int` into a 64 long character.
  ! -------------------------------------------------------------------------- !
  character(len=MAX_LEN) function castIntPtrToChar(intPtr, castStat)
    integer, pointer,  intent(in) :: intPtr
      !! Integer to cast to character.
    integer, optional, intent(out) :: castStat
      !! Casting status.

    integer :: status

    if (associated(intPtr)) then
      ! Initialize the output.
      castIntPtrToChar = ""

      write(castIntPtrToChar, DEF_INT_FORMAT, iostat=status) intPtr
    else
      call raiseError("The 'int' dummy argument is not associated " // &
          "with any target.")
    end if

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (status /= 0) &
          call raiseError("Casting from int pointer to char failed.")
    end if
  end function castIntPtrToChar


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castCharToReal
  !>  Cast the character `char` into a real number of default kind.
  ! -------------------------------------------------------------------------- !
  real function castCharToReal(char, castStat)
    character(len=*), intent(in) :: char
      !! Character input to be casted to real.
    integer, optional, intent(out) :: castStat
      !! Casting status.
    
    integer :: status
    read(char, DEF_REAL_FORMAT, iostat=status) castCharToReal

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (castStat /= 0) &
          call raiseError("'" // trim(char) // "' is not numeric.")
    end if
  end function castCharToReal


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castRealToChar
  !>  Cast the real number `realNum` into a 64 long character.
  ! -------------------------------------------------------------------------- !
  character(len=MAX_LEN) function castRealToChar(realNum, castStat)
    real,              intent(in) :: realNum
      !! Real number to cast to character.
    integer, optional, intent(out) :: castStat
    
    integer :: status
    write(castRealToChar, DEF_REAL_FORMAT, iostat=status) realNum

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (status /= 0) call raiseError("Casting real to char failed.")
    end if
  end function castRealToChar
end module CastProcedures

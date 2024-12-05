module CastProcs
  ! -------------------------------------------------------------------------- !
  ! MODULE: CastProcedures
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for casting values from one data type 
  !!  to another.
  ! -------------------------------------------------------------------------- !
  use ErrorMSG, only: raiseError
  use, intrinsic :: iso_fortran_env, only: real64, real32
  use, intrinsic :: ieee_arithmetic
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
  public :: castReal32ToChar
  public :: castReal64ToChar
  public :: castCharToReal32
  public :: castCharToReal64
  public :: logicalToInt
  public :: isFinite32
  public :: isFinite64
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
    ! Initialize status.
    status = 0

    read(char, *, iostat=status) castCharToInt

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
  function castIntToChar(int, castStat) result(charInt)
    integer,           intent(in) :: int
      !! Integer to cast to character.
    integer, optional, intent(out) :: castStat
      !! Casting status.

    character(len=:), allocatable :: charInt
    character(len=MAX_LEN) :: rawCharInt

    integer :: status

    ! Initialize status.
    status = 0
    ! Initialize output.
    allocate(character(len=0) :: charInt)

    write(rawCharInt, DEF_INT_FORMAT, iostat=status) int

    ! Trim whitespaces and adjust character to the left.
    if (status == 0) charInt = trim(adjustl(rawCharInt))

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

    ! Initialize status.
    status = 0

    if (associated(intPtr)) then
      write(castIntPtrToChar, DEF_INT_FORMAT, iostat=status) intPtr

      ! Adjust character to the left.
      if (status == 0)  castIntPtrToChar = adjustl(castIntPtrToChar)
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
  !>  Cast the character `char` into a real32.
  ! -------------------------------------------------------------------------- !
  real(kind=real32) function castCharToReal32(char, castStat)
    character(len=*),  intent(in) :: char
      !! Character input to be casted to real.
    integer, optional, intent(out) :: castStat
      !! Casting status.
    
    integer :: status
    ! Initialize status.
    status = 0

    read(char, *, iostat=status) castCharToReal32

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (status /= 0) &
          call raiseError("'" // trim(char) // "' is not numeric.")
    end if
  end function castCharToReal32


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castCharToReal64
  !>  Cast the character `char` into real64.
  ! -------------------------------------------------------------------------- !
  real(kind=real64) function castCharToReal64(char, castStat)
    character(len=*),  intent(in) :: char
      !! Character input to be casted to real.
    integer, optional, intent(out) :: castStat
      !! Casting status.
    
    integer :: status
    ! Initialize status.
    status = 0

    read(char, *, iostat=status) castCharToReal64

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (status /= 0) &
          call raiseError("'" // trim(char) // "' is not numeric.")
    end if
  end function castCharToReal64


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castReal32ToChar
  !>  Cast the real32 number `realNum` into a 64 long character.
  ! -------------------------------------------------------------------------- !
  function castReal32ToChar(realNum, castStat) result(charReal)
    real(kind=real32), intent(in) :: realNum
      !! Real number to cast to character.
    integer, optional, intent(out) :: castStat

    character(len=:), allocatable :: charReal
    character(len=MAX_LEN) :: rawCharReal
    
    integer :: status

    ! Initialize status.
    status = 0

    ! Initialize output.
    allocate(character(len=0) :: charReal)

    write(rawCharReal, DEF_REAL_FORMAT, iostat=status) realNum
    
    ! Trim whitespaces and adjust character to the left.
    if (status == 0)  charReal = trim(adjustl(rawCharReal))

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (status /= 0) call raiseError("Casting real to char failed.")
    end if
  end function castReal32ToChar


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castReal64ToChar
  !>  Cast the real64 number `realNum` into a 64 long character.
  ! -------------------------------------------------------------------------- !
  function castReal64ToChar(realNum, castStat) result(charReal)
    real(kind=real64), intent(in) :: realNum
      !! Real number to cast to character.
    integer, optional, intent(out) :: castStat

    character(len=:), allocatable :: charReal
    character(len=MAX_LEN) :: rawCharReal
    
    integer :: status

    ! Initialize status.
    status = 0

    ! Initialize output.
    allocate(character(len=0) :: charReal)

    write(rawCharReal, DEF_REAL_FORMAT, iostat=status) realNum
    
    ! Trim whitespaces and adjust character to the left.
    if (status == 0)  charReal = trim(adjustl(rawCharReal))

    ! Let other routines to handle casting errors.
    if (present(castStat)) then
      castStat = status
    else
      ! By default, stop the program if casting failed.
      if (status /= 0) call raiseError("Casting real to char failed.")
    end if
  end function castReal64ToChar


  ! -------------------------------------------------------------------------- !
  ! FUNCTION logicalToInt
  !>  Transform TRUE into 1 and FALSE into 0. Being an elemental function,
  !!  it can also accept array dummy arguments.
  ! -------------------------------------------------------------------------- !
  pure elemental integer function logicalToInt(input) result(output)
    logical, intent(in) :: input
    if (input) then
      output = 1
    else
      output = 0
    end if
  end function logicalToInt


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isFinite32
  !>  Determine if the input real32 value is finite, i.e. not NaN 
  !!  (signaling and quiet) or infinity. Note that this is a wrapper for
  !!  several procedures from IEEE_ARITHMETIC intrinsic module
  ! -------------------------------------------------------------------------- !
  pure logical function isFinite32(x)
    real(kind=real32), intent(in) :: x
    isFinite32 = ( &
      ieee_class(x) /= ieee_quiet_nan     .and. &
      ieee_class(x) /= ieee_signaling_nan .and. &
      ieee_class(x) /= ieee_positive_inf  .and. &
      ieee_class(x) /= ieee_negative_inf        &
    )
  end function isFinite32


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isFinite64
  !>  Determine if the input real32 value is finite, i.e. not NaN 
  !!  (signaling and quiet) or infinity. Note that this is a wrapper for
  !!  several procedures from IEEE_ARITHMETIC intrinsic module
  ! -------------------------------------------------------------------------- !
  pure logical function isFinite64(x)
    real(kind=real64), intent(in) :: x
    isFinite64 = ( &
      ieee_class(x) /= ieee_quiet_nan     .and. &
      ieee_class(x) /= ieee_signaling_nan .and. &
      ieee_class(x) /= ieee_positive_inf  .and. &
      ieee_class(x) /= ieee_negative_inf        &
    )
  end function isFinite64
end module CastProcs

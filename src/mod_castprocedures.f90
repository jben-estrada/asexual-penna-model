module CastProcedures
  ! -------------------------------------------------------------------------- !
  ! MODULE: CastProcedures
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for casting values from one data type 
  !   to another.
  ! -------------------------------------------------------------------------- !
  implicit none
  private
  
  integer, parameter :: MAX_LEN = 64
    !! Maximum character length for x-to-char procedures.

  public :: castCharToInt
  public :: castIntToChar
  public :: castIntPtrToChar
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: castCharToInt
  !>  Cast the input character `char` into an integer.
  ! -------------------------------------------------------------------------- !
  integer function castCharToInt(char, caststat)
    character(len=*), intent(in) :: char
      !! Character to cast to integer.
    integer, optional, intent(out) :: castStat
    !! Casting status. A value of 0 means

    integer :: status
    read(char, *, iostat=status) castCharToInt

    ! Handle casting error in other procedures/program.
    if (present(castStat)) then
      castStat = status
    else

      ! Handle casting error in this function.
      if (status /= 0) then
        print "(*(a))", "***ERROR. '", trim(adjustl(char)), "' is not numeric."
        error stop
      end if
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
      !! Casting status. A value of 0 means

    integer :: status

    ! Initialize the output.
    castIntToChar = ""

    write(castIntToChar, *, iostat=status) int

    ! Handle casting error in other procedures/program.
    if (present(castStat)) then
      castStat = status
    else
      
      ! Handle casting error in this function.
      if (status /= 0) then
        print "(a, i0, a)", "***ERROR. '", int, &
            "' cannot be casted to character."
        error stop
      end if
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
      !! Casting status. A value of 0 means

    integer :: status

    if (associated(intPtr)) then
      ! Initialize the output.
      castIntPtrToChar = ""

      write(castIntPtrToChar, *, iostat=status) intPtr
    else
      print "(a)", "***ERROR. The 'int' dummy argument is not associated " // &
          "with any target."
      error stop  
    end if

    ! Handle casting error in other procedures/program.
    if (present(castStat)) then
      castStat = status
    else
      
      ! Handle casting error in this function.
      if (status /= 0) then
        print "(a, i0, a)", "***ERROR. '", intPtr, &
            "' cannot be casted to character."
        error stop
      end if
    end if
  end function castIntPtrToChar
end module CastProcedures

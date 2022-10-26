module ANSIColorCodes
  ! -------------------------------------------------------------------------- !
  ! MODULE: ANSIColorCodes
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for the formatting of text attributes of 
  !!  characters to be displayed on screen with ANSI escape codes
  !!  (SGR, specifically)
  !!
  !!  Based from the `ansi_colors` module by Jason Blevins in
  !!  http://fortranwiki.org/fortran/show/ansi_colors
  ! -------------------------------------------------------------------------- !
  implicit none
  private

  ! Parts of ANSCI escape codes: start and end sequences.
  ! Syntax: <ESC_CODE_START><code><ESC_CODE_END>
  ! where <code> is a character of integers.
  character(len=*), parameter :: ESC_CODE_START = achar(27) // "["
  character(len=*), parameter :: ESC_CODE_END = "m"   ! Graphics mode
  character(len=*), parameter :: ESC_CODE_RESET = "0"

  ! Parts of ANSCI escape codes: color.
  character(len=*), parameter :: ESC_CODE_RED = "31"
  character(len=*), parameter :: ESC_CODE_GREEN = "32"
  character(len=*), parameter :: ESC_CODE_YELLOW = "33"
  character(len=*), parameter :: ESC_CODE_BLUE = "34"
  character(len=*), parameter :: ESC_CODE_MAGENTA = "35"
  character(len=*), parameter :: ESC_CODE_CYAN = "36"

  ! Parts of ANSCI escape codes: text attributes.
  character(len=*), parameter :: ESC_CODE_BOLD = "1"
  character(len=*), parameter :: ESC_CODE_UNDERSCORE = "4"
  character(len=*), parameter :: ESC_CODE_BLINK = "5"
  character(len=*), parameter :: ESC_CODE_REVERSE = "7"
  character(len=*), parameter :: ESC_CODE_HIDE = "8"

  ! Formatting flags: color.
  character(len=*), public, parameter :: escCodeRed = "r"
  character(len=*), public, parameter :: escCodeGreen = "g"
  character(len=*), public, parameter :: escCodeYellow = "y"
  character(len=*), public, parameter :: escCodeBlue = "b"
  character(len=*), public, parameter :: escCodeMagenta = "m"
  character(len=*), public, parameter :: escCodeCyan = "c"

  ! Formatting flags: text attribute.
  character(len=*), public, parameter :: escCodeBold = "B"
  character(len=*), public, parameter :: escCodeUndrScr = "U"
  character(len=*), public, parameter :: escCodeBlink = "L"
  character(len=*), public, parameter :: escCodeReverse = "R"
  character(len=*), public, parameter :: escCodeHide = "H"

  public :: getEscCode
  public :: resetCode
  public :: formatChar
contains
  

  ! -------------------------------------------------------------------------- !
  ! FUNCTION: resetCode
  !>  Get the ANSI escape code for resetting the formatting back to the 
  !!  default one.
  ! -------------------------------------------------------------------------- !
  function resetCode() result(code)
    character(len=:), allocatable :: code
    code = ESC_CODE_START // ESC_CODE_RESET // ESC_CODE_END
  end function resetCode


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCodeNum
  !>  Get the number for the ANSI escape code specified by the character 
  !!  argument `flag`.
  ! -------------------------------------------------------------------------- !
  recursive function getCodeNum(flag) result(code)
    character(len=*), intent(in)  :: flag
      !! Formatting flag.
    character(len=:), allocatable :: code
      !! The ANSI escape code number requested.
    
    select case(flag)
      case(escCodeRed)
        code = ESC_CODE_RED
      case(escCodeGreen)
        code = ESC_CODE_GREEN
      case(escCodeYellow)
        code = ESC_CODE_YELLOW
      case(escCodeBlue)
        code = ESC_CODE_BLUE
      case(escCodeMagenta)
        code = ESC_CODE_MAGENTA
      case(escCodeCyan)
        code = ESC_CODE_CYAN
      case(escCodeBold)
        code = ESC_CODE_BOLD
      case(escCodeUndrScr)
        code = ESC_CODE_UNDERSCORE
      case(escCodeBlink)
        code = ESC_CODE_BLINK
      case(escCodeReverse)
        code = ESC_CODE_REVERSE
      case(escCodeHide)
        code = ESC_CODE_HIDE
      case default
        print "(2a)", &
            formatChar("***ERROR***", escCodeRed // escCodeBold), &
            formatChar("'"// trim(flag) // &
                "' is an invalid flag for an ANSI escape code.", escCodeRed)
        error stop
    end select
  end function getCodeNum


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getEscCode
  !>  Get the ANSI escape code specified by the character argument `flag`.
  ! -------------------------------------------------------------------------- !
  function getEscCode(flag) result(code)
    character(len=*), intent(in)  :: flag
      !! Formatting flag.
    character(len=:), allocatable :: code
      !! The ANSI escape code requested.
    
    code = ESC_CODE_START // getCodeNum(flag) // ESC_CODE_END
  end function getEscCode


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: formatChar
  !>  Change the color and/or text attributes of the passed character argument 
  !!  `char` with formatting flags `flags`. Multiple flags are appended
  !!  together.
  ! -------------------------------------------------------------------------- !
  recursive function formatChar(char, flags) result(formatted)
    character(len=*), intent(in)  :: char
      !! Character to be formatted.
    character(len=*), intent(in)  :: flags
      !! Format flags to specify which formatting are to be applied.
    character(len=:), allocatable :: formatted
      !! The formatted input `char`.

    character :: currChar
    integer   :: i

    ! Initialize tthe output.
    formatted = ESC_CODE_START
  
    do i = 1, len(flags)
      currChar = flags(i:i)
      formatted = formatted // getCodeNum(currChar)

      if (i < len(flags)) then
        ! Append separator.
        formatted = formatted // ";"
      else if (i == len(flags)) then
        ! Append the character to format and the reset formatting ANSI code.
        ! NOTE: Move this bit inside the do loop so as gfortran would shut it
        ! with its 'Wmaybe-uninitialized' warning.
        formatted = formatted // ESC_CODE_END // char // resetCode()
      end if
    end do
  end function formatChar
end module ANSIColorCodes

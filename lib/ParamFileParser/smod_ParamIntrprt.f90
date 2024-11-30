submodule (ParamFileParserType) ParamIntrprt
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: ParamIntrprt
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `ParamFileParserType` containing procedures for parsing
  !!  external files to obtain parameters.
  ! -------------------------------------------------------------------------- !
  implicit none

  character, parameter :: ARR_DELIM = ","
    !! Array delimiter.
  character, parameter :: ARR_L_BRCKT = "["
    !! Left bracket of arrays.
  character, parameter :: ARR_R_BRCKT = "]"
    !! Right bracket of arrays.
  character(len=3), parameter :: ELLIPSIS = "..."
    !! Ellipsis for automatic assignment of array elements.
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: paramInt
  !>  Convert the character `paramVal` to integer.
  ! -------------------------------------------------------------------------- !
  integer function paramInt(paramVal)
    character(len=*), intent(in) :: paramVal
      !! Character to be inspected and casted to integer.

    ! Integer delimiter.
    character, parameter :: INT_DELIM = "_"

    character(len=:), allocatable :: cleanParamVal

    character :: currChar
    integer   :: i

    ! Initialize output.
    paramInt = 0

    ! Initialize local variables.
    allocate(character(len=0) :: cleanParamVal)

    do i = 1, len(trim(adjustl(paramVal)))
      currChar = paramVal(i:i)

      ! Only get numeric characters.
      if (isDigit(currChar)) then
        cleanParamVal = cleanParamVal // currChar

      ! Skip integer delimiter.
      else if (currChar == INT_DELIM) then
        cycle

      ! Skip whitespace characters.
      else if (isWhiteSpace(currChar)) then
        cycle

      else
        call raiseError("'" // trim(adjustl(paramVal)) // &
          "' cannot be interpreted as an integer.")
      end if
    end do

    paramInt = castCharToInt(cleanParamVal)

    ! Free allocated memory.
    deallocate(cleanParamVal)
  end function paramInt


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: paramReal
  !>  Convert the character `paramVal` to real.
  ! -------------------------------------------------------------------------- !
  real function paramReal(paramVal)
    character(len=*), intent(in) :: paramVal
      !! Character to be inspected and casted to real.

    ! No need for extra syntax check; syntax for valid char for casting to real
    ! is handled by Fortran already.
    paramReal = castCharToReal(paramVal)
  end function paramReal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramScalar
  !>  Convert the character `paramVal` to either integer or real, the type of
  !!  which is determined by the type of the output argument `scalar`.
  ! -------------------------------------------------------------------------- !
  module subroutine paramScalar(paramVal, scalar)
    character(len=*), intent(in)  :: paramVal
      !! Character to be inspected and casted to either integer or real.
    class(*),         intent(out) :: scalar
      !! Output scalar value.

    select type(scalar)
      type is (integer)
        scalar = paramInt(paramVal)
      type is (real)
        scalar = paramReal(paramVal)
      class default
        call raiseError("Invalid scalar type. Must be either integer or real.")
    end select
  end subroutine paramScalar


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isValidEllipsis
  !>  Check if `paramVal` has valid syntax for ellipsis.
  ! -------------------------------------------------------------------------- !
  logical function isValidEllipsis(paramVal, paramValLen, charIdx)
    character(len=*), intent(in)    :: paramVal
      !! Character to be inspected.
    integer,          intent(in)    :: paramValLen
      !! Length of trimmed `paramVal`.
    integer,          intent(inout) :: charIdx
      !! Character index.

    character :: currChar
    integer   :: tempCharIdx

    ! Initialize output.
    isValidEllipsis = .false.

    ! Character index bounds. Minimum substring length must be 4.
    if (charIdx + 3 > paramValLen) return

    ! Check ellipsis character.
    if (paramVal(charIdx: charIdx+2) /= ELLIPSIS) return

    ! Check if ellipsis is at the end of the parameter array.
    tempCharIdx = charIdx + 3
    do while(tempCharIdx <= paramValLen)
      currChar = paramVal(tempCharIdx: tempCharIdx)

      ! Ignore whitespace characters.
      if (isWhiteSpace(currChar)) then
        tempCharIdx = tempCharIdx + 1
        cycle

      ! Check for right bracket.
      else if (currChar == ARR_R_BRCKT) then
        isValidEllipsis = .true.
        charIdx = tempCharIdx  ! Update character index.
        exit

      else
        call raiseError("Junk character '" // currChar //"' within array")
      end if
    end do

    if (.not. isValidEllipsis) then
      call raiseError( &
        "Ellipses must be at the end of arrays. " &
        // "Array in question: '" &
        // trim(adjustl(paramVal)) // "'." &
        )
    end if
  end function isValidEllipsis


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramArray
  !>  Convert `paramVal` to array whose type is either integer or real.
  !!  The type of the output array `array` is determined by its type.
  ! -------------------------------------------------------------------------- !
  module subroutine paramArray(paramVal, array)
    character(len=*), intent(in)  :: paramVal
      !! Character to be inspected and casted to integer array.
    class(*),         intent(out) :: array(:)
      !! Array output. Its type must be either integer or real.

    character(len=:), allocatable :: arrElem  ! Array element.
    character :: currChar
    integer   :: currArrPart  ! Current part of the array being inspected.
    integer   :: charIdx, arrIdx, paramValLen, arrSize

    ! Array parts.
    integer, parameter :: ARR_PART_LEFT = 0    ! Currently left of the array
    integer, parameter :: ARR_PART_INSIDE = 1  ! Currently in the array
    integer, parameter :: ARR_PART_RIGHT = 2   ! Currently right of the array

    ! Initialize local variables.
    allocate(character(len=0) :: arrElem)
    currArrPart = ARR_PART_LEFT
    arrIdx = 1
    charIdx = 1
    paramValLen = len(trim(paramVal))
    arrSize = size(array)

    do while(charIdx <= paramValLen)
      currChar = paramVal(charIdx: charIdx)

      select case(currArrPart)
        case(ARR_PART_LEFT)
          if (isWhiteSpace(currChar)) then
            ! Ignore initial whitespace characters. Do nothing here.
          else if (currChar == ARR_L_BRCKT) then
            ! Switch to reading array elements once the left bracket
            ! is reached.
            currArrPart = ARR_PART_INSIDE
          else
            call raiseError(&
              "Invalid array syntax for '" &
              // trim(adjustl(paramVal)) // &
              "'. Must begin with '" &
              // ARR_L_BRCKT // "'."&
              )
          end if


        case(ARR_PART_INSIDE)
          if (isWhiteSpace(currChar)) then
            ! Ignore whitespace characters. Do nothing here.
          else if (currChar == ARR_DELIM) then

            ! Accept non-empty array elements. Zero-length array elements
            ! mean that there are consecutive array delimiters (e.g. [1,,2])
            ! which is forbidden.
            if (len(arrElem) == 0) then
              call raiseError( &
                "Invalid array syntax for '" &
                // trim(adjustl(paramVal)) // &
                "'. Imbalanced array delimiter '" &
                // ARR_DELIM // "'." &
                )
            end if

            ! Assign array element to the output array.
            call assignToArray(arrElem, arrIdx, arrIdx)

            ! Increment array index.
            arrIdx = arrIdx + 1

            ! Clear temporary character.
            deallocate(arrElem)
            allocate(character(len=0) :: arrElem)

          else if (currChar == ARR_R_BRCKT) then
            ! Both the sizes of the passed array and the array in the external
            ! file must be equal.
            if (arrIdx /= arrSize) then
              call raiseError( &
                "Parameter array must of the same size with the output " // &
                "array of size " // castIntToChar(arrSize) // "." &
                // new_line("") // &
                "Parameter array: " // trim(adjustl(paramVal)) // &
                " (size = " // castIntToChar(arrIdx) // ")" &
                )
            end if

            ! Assign the last array element to the output array.
            call assignToArray(arrElem, arrIdx, arrIdx)

            ! Switch to reading characters right of the array.
            currArrPart = ARR_PART_RIGHT

          else if (currChar == ELLIPSIS(1:1)) then
            if (isValidEllipsis(paramVal, paramValLen, charIdx)) then
              ! Check for consecutive array delimiters.
              if (len(arrElem) == 0) then
                call raiseError( &
                  "Invalid array syntax for '" &
                  // trim(adjustl(paramVal)) // &
                  "'. Imbalanced array delimiter '" // ARR_DELIM // "'." &
                  )
              end if

              ! Assign the remaining uninitialized elements of the output
              ! array with `arrElem`.
              call assignToArray(arrElem, arrIdx, arrSize)

              ! Switch to reading outside of the array.
              currArrPart = ARR_PART_RIGHT
            else
              ! The period could be part of a real number (decimal point).
              select type(array)
                type is (real)
                  arrElem = arrElem // currChar
                class default
                  call raiseError( &
                    "Invalid character '" &
                    // currChar // &
                    "' in the integer array '" &
                    // trim(paramVal) // "'." &
                    )
              end select
            end if

          else
            ! Append anything else to the temporary character variable.
            arrElem = arrElem // currChar
          end if


        case(ARR_PART_RIGHT)
          ! Check for junk outside the array.
          if (isWhiteSpace(currChar)) then
            ! Do nothing for whitespace characters.
          else
            ! Should there be non-whitespace characters outside the array
            ! raise an error.
            call raiseError( &
              "Junk character '" &
              // currChar // &
              "' next to the array '" &
              // trim(adjustl(paramVal)) // "'." &
              )
          end if


        case default
          call raiseError("Internal error. Unspecified array part.")
      end select

      ! Proceed to the next character.
      charIdx = charIdx + 1
    end do

    ! Check if array readiing did not end with the ']' character.
    if (currArrPart /= ARR_PART_RIGHT) then
      call raiseError( & 
        "Invalid array syntax for '" &
        // trim(paramVal) // &
        "'. It must end with '" &
        // ARR_R_BRCKT // "'." &
        )
    end if
  contains


    ! ----------------------------------------------------------------------- !
    ! SUBROUTINE: assignToArray
    !>  Cast `elemChar` to integer, real or character, and assign it to the
    !!  slice of range [lowIdx, hiIdx] of non-local variable `array`.
    ! ----------------------------------------------------------------------- !
    subroutine assignToArray(elemChar, lowIdx, hiIdx)
      character(len=*), intent(in)    :: elemChar
        !! Character to be casted to real.
      integer,          intent(in)    :: lowIdx
        !! The lower index of the range in which `elemChar` is to be stored.
      integer,          intent(in)    :: hiIdx
        !! The higher index of the range in which `elemChar` is to be stored.

      ! Check first all invalid index choices.
      if (1 > lowIdx .or. hiIdx > arrSize) then
        call raiseError( &
          "Range [" // castIntToChar(lowIdx) // ":" // &
          castIntToChar(hiIdx) // "] out of array bounds. " // &
          "(array size: " // castIntToChar(arrSize) // ")" &
          )
      else if (lowIdx > hiIdx) then
        call raiseError( &
          "Lower index is larger than the higher index. (" &
          // castIntToChar(lowIdx) // ">" &
          // castIntToChar(hiIdx) // ")" &
          )
      end if

      select type(array)
        type is (integer)
          array(lowIdx: hiIdx) = paramInt(elemChar)
        type is (real)
          array(lowIdx: hiIdx) = paramReal(elemChar)
        type is (character(len=*))
          array(lowIdx: hiIdx) = elemChar
        class default
          call raiseError("Invalid array type. Must be either integer or real.")
      end select
    end subroutine assignToArray
  end subroutine paramArray
end submodule ParamIntrprt

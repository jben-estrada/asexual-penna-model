submodule (ModelParam) ReadProcedures
  use RNG, only: RNG_INTRINSIC
  use SaveFormat, only: nullFlag
  implicit none

  ! -------------------------------------------------------------------------- !
  ! Parameter keys. NOTE: Padded with spaces to accept initializer.
  character(len=*), parameter :: MODEL_PARAM_KEYS(MODEL_PARAM_COUNT) = &
    ["L          ", &
     "T          ", &
     "B          ", &
     "M          ", &
     "R          ", &
     "R_max      ", &
     "K          ", &
     "N0         ", &
     "t_max      ", &
     "sample_size", &
     "rec_flag   ", &
     "rng        ", &
     "seed       "]

  ! -------------------------------------------------------------------------- !
  ! Internal default parameter values. If all things go wrong, this is used
  ! instead. NOTE: The order is the same with `MODEL_PARAM_KEYS`.
  integer, parameter :: MODEL_PARAM_DEFAULT(MODEL_PARAM_COUNT) = &
      [32, 3, 1, 1, 9, 9, 20000, 100, 100, 1, nullFlag, RNG_INTRINSIC, 1]


  ! -------------------------------------------------------------------------- !
  ! Units for writing on files.
  integer, parameter :: MODEL_UNIT = 99
  integer, parameter :: VWEIGHT_UNIT = 98


  ! NON-LITERAL CHARACTERS.
  ! -------------------------------------------------------------------------- !
  ! Comment character.
  character, parameter :: COMMENT = ";"
  ! Key-value separator.
  character, parameter :: KEYVAL_SEP = "="
  ! Verhulst weight separator.
  character, parameter :: VWEIGHT_SEP = ","
  ! End of line character.
  character, parameter :: EOL = "/"
  ! Default null value. This could be any non-positive integers.
  integer, parameter :: NULL_VALUE = -1
  ! Ignore value. This could also be any non-positive integers.
  integer, parameter :: IGNORE_VALUE = 0
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readScalarModelParamCfg
  !>  Read the scalar model parameters from an external file.
  ! -------------------------------------------------------------------------- !
  subroutine readScalarModelParamCfg()
    integer :: values(MODEL_PARAM_COUNT)
    integer :: fileStatus
    integer :: keyIdx
    integer :: val

    character(len=:), allocatable :: strippedFile
    character(len=:), allocatable :: key
    character(len=:), allocatable :: strVal
    character :: currChar
    integer   :: charNum
    integer   :: castStatus
    logical   :: isReadingKey
    integer   :: i
    
    ! Assign default values.
    values(:) = MODEL_PARAM_DEFAULT

    ! Read file.
    open(unit=MODEL_UNIT, file=MODEL_FILE_NAME, status='old', iostat=fileStatus)

    ! Warn missing file. TODO: Make better warning messages.
    if (fileStatus /= 0) then
      print "(a/, 3a)", "***", "WARNING. Cannot read '", trim(MODEL_FILE_NAME),&
          "'. Using the following default values:"
      call printModelParam()
      modelParams(:) = MODEL_PARAM_DEFAULT(:)

      return
    end if

    ! Clean model config file.
    call stripFile(strippedFile, MODEL_UNIT)    
    close(MODEL_UNIT)

    ! Evaluate file.
    allocate(character(len=0) :: strVal)
    isReadingKey = .true.
    key = ""
    do charNum = 1, len(strippedFile)
      currChar = strippedFile(charNum:charNum)

      ! Check change in read state.
      select case (currChar)
        ! ***KEYVAL_SEP: Shift from reading LHS to RHS.
        case (KEYVAL_SEP)
          isReadingKey = .false.
          strVal = ""
          cycle

        ! ***EOL: Shift from reading RHS to LHS.
        case (EOL)
          isReadingKey = .true.

          ! Read previous key and value.
          keyIdx = getCharArrayIndex(key)
          select case (keyIdx)
            case (IGNORE_VALUE)
              key = ""
              cycle
            case (NULL_VALUE)
              print "(3(a))", "***WARNING. '", key, &
                  "' is not a valid parameter. Ignoring this key."
              print "(a/, 10(' '), *(a, ', '))", "Valid parameters:", &
                  (trim(MODEL_PARAM_KEYS(i)), i = 1, size(MODEL_PARAM_KEYS))
              key = ""
              cycle
            case default
              ! Cast the value string to integer.
              read(strVal, *, iostat=castStatus) val
              if (castStatus == 0) then
                values(keyIdx) = val
              else
                print "(3(a))", "***WARNING. '", strVal, "' is not a valid" // &
                    " value."
              end if
              key = ""
          end select
        
        ! ***Neither case.
        case default
          ! 'Eat' input.
          if (isReadingKey) then
            key = key // currChar
          else
            strVal = strVal // currChar
          end if
      end select
    end do

    modelParams(:) = values(:)
    deallocate(strippedFile)
    deallocate(key)
    deallocate(strVal)
  end subroutine readScalarModelParamCfg


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printModelParam
  !>  Print the model parameters for error/warning messages.
  ! -------------------------------------------------------------------------- !
  subroutine printModelParam()
    integer :: i

    do i = 1, MODEL_PARAM_COUNT
      print "(4(' '), a12, ': ', i0)", MODEL_PARAM_KEYS(i), &
          MODEL_PARAM_DEFAULT(i)
    end do

    ! Print separator.
    print "(a)", "***"
  end subroutine printModelParam


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: stripFile
  !>  Read the specified file and strip whitespaces and
  !!  comments (lines starting with ';').
  ! -------------------------------------------------------------------------- !
  subroutine stripFile(strippedFile, unit)
    character(len=:), allocatable, intent(out) :: strippedFile
    integer,                       intent(in)  :: unit

    character(len=:), allocatable :: line
    character(len=MAX_LEN)        :: rawLine
    character :: currChar
    integer   :: readStatus
    integer   :: charNum

    allocate(character(len=0) :: strippedFile)
    allocate(character(len=0) :: line)
    rawLine = ""
    do
      ! Read line.
      read(unit, "(a)", iostat=readStatus) rawLine
      if (readStatus == 0) then
        line = trim(rawLine)
      else
        exit
      end if

      ! Strip whitespaces.
      do charNum = 1, len(line)
        currChar = line(charNum:charNum)
        
        if (currChar == " ") then
          cycle
        else if (currChar == COMMENT) then
          strippedFile = strippedFile // EOL
          exit
        end if

        strippedFile = strippedFile // currChar
        if (charNum == len(line)) strippedFile = strippedFile // EOL
      end do
    end do

    deallocate(line)
  end subroutine stripFile


  
  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCharArrayIndex
  !>  Get the corresponding index of `elem` in a rank-1 array of 
  !!  characters `elem`. If `elem` is not found in `array`, it
  !!  returns `NULL_VALUE` which is set to 0.
  ! -------------------------------------------------------------------------- !
  function getCharArrayIndex(elem) result(i)
    character(len=:), allocatable, intent(in) :: elem

    integer :: i

    do i = 1, MODEL_PARAM_COUNT
      if (MODEL_PARAM_KEYS(i) == elem) return
    end do

    if (elem == "") then
      i = IGNORE_VALUE
    else
      i = NULL_VALUE
    end if
  end function getCharArrayIndex



  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readVerhulstWeightsCfg
  !>  Read the Verhulst weights values from a .ini file.
  ! -------------------------------------------------------------------------- !
  subroutine readVerhulstWeightsCfg()
    integer :: fileStatus
    integer :: readStatus
    integer :: charNum
    integer :: vWeightIdx
    real    :: vWeight

    character(len=:), allocatable :: strippedFile
    character(len=:), allocatable :: vWeightStr
    character                     :: currChar

    ! Initialize Verhulst weight array.
    if (.not.allocated(MODEL_VERHULST_W)) allocate(MODEL_VERHULST_W(MODEL_L))
    MODEL_VERHULST_W(:) = VWEIGHT_DEFAULT

    ! Read file.
    allocate(character(len=0) :: strippedFile)
    open(unit=VWEIGHT_UNIT, file=VWEIGHT_FILE_NAME, status='old', &
        iostat=fileStatus)

    ! Warn missing file. TODO: Make better warning messages.
    if (fileStatus /= 0) then
      print "(a/, 3(a))", "***", &
          "WARNING. Cannot read '", & 
          trim(VWEIGHT_FILE_NAME), &
          "'. Using the following default value:"
      print "(/a, i0, /a, f4.2)", "age:    1-", MODEL_L, "weight: ", &
          VWEIGHT_DEFAULT

      ! Print separator.
      print "(a)", "***"
      return
    end if
    
    ! Clean Verhulst weight config file. 
    call stripFile(strippedFile, VWEIGHT_UNIT)
    close(VWEIGHT_UNIT)

    ! Evaluate stripped input.
    vWeightIdx = 1
    allocate(character(len=0) :: vWeightStr)
    do charNum = 1, len(strippedFile)
      currChar = strippedFile(charNum:charNum)

      ! > Check read state. (NOTE: `>` are visual hints for nasty nested if's)
      ! ======================================
      ReadCheck: if (currChar == VWEIGHT_SEP &
                     .or. charNum == len(strippedFile)) then
        ! Cast string to real.
        read(vWeightStr, *, iostat=readStatus) vWeight

        ! >> Check if the char-to-real casting succeeds.
        ! ===========================================
        CastCheck: if (readStatus == 0) then

          ! >>> Check all possible errors.
          ! ==============================================================
          AssignWeight: if (vWeightIdx <= MODEL_L .and. vWeight <= 1) then
            MODEL_VERHULST_W(vWeightIdx) = vWeight

          ! ***WARNING. Invalid number of weights.
          else if (vWeightIdx > MODEL_L) then
            print "(a)", "***WARNING. Given Verhulst weights exceeded the " // &
                "maximum number of allowed number of weights."

          ! ***WARNING. Invalid range.
          else if (vWeight > 1) then
            print "(a, f5.3, a)", "***WARNING. Given Verhulst weight" // &
                "is outside the allowed range [0, 1]. Using the " // &
                "default value (", VWEIGHT_DEFAULT, ")."

          ! ***Unknown error.
          else
            stop "***ERROR. Unknown error when reading Verhulst weights."
          end if AssignWeight
          ! <<<
          ! ==============================================================

        ! ***WARNING. Invalid input.
        else
          print "(3(a), f5.3, a)", "***WARNING. '", vWeightStr , &
              "' is not a valid value for a Verhulst factor. " // &
              "Using the default value (", VWEIGHT_DEFAULT, ")."
        end if CastCheck
        ! <<
        ! ===========================================

        vWeightIdx = vWeightIdx + 1
        vWeightStr = ""
      ! Continue reading values for Verhulst weights.
      else
        if (isNumeric(currChar) .or. currChar == ".") &
            vWeightStr = vWeightStr // currChar
      end if ReadCheck
      ! <
      ! ======================================
    end do

    deallocate(vWeightStr)
  end subroutine readVerhulstWeightsCfg


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isNumeric
  !>  Check whether the character `char` is a number or not.
  ! -------------------------------------------------------------------------- !
  logical function isNumeric(char)
    character, intent(in) :: char

    integer :: asciiNum
  
    ! NOTE: ASCII characters from 48 to 57 are the numbers 0 to 9.
    asciiNum = iachar(char)
    isNumeric = (48 <= asciiNum .and. asciiNum <= 57)
  end function isNumeric
end submodule

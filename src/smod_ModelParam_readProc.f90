submodule (ModelParam) ReadProcedures
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: ReadProcedures
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `ModelParam` containing procedures for reading files of
  !!  model parameters and "Verhulst weight".
  ! -------------------------------------------------------------------------- !
  implicit none

  ! -------------------------------------------------------------------------- !
  ! Parameter keys. NOTE: Padded with spaces to accept initializer.
  character(len=*), parameter :: PARAM_KEYS(PARAM_COUNT) = &
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
     "seed       ", &
     "mttn_count "]

  logical :: PARAM_ASSIGNED(PARAM_COUNT) = .false.

  ! -------------------------------------------------------------------------- !
  ! Units for writing on files.
  integer, parameter :: MODEL_UNIT = 99
  integer, parameter :: VWEIGHT_UNIT = 98


  ! RESERVED CHARACTERS.
  ! -------------------------------------------------------------------------- !
  ! Comment character.
  character, parameter :: COMMENT = ";"
  ! Key-value separator.
  character, parameter :: KEYVAL_SEP = "="
  ! Verhulst weight separator.
  character, parameter :: VWEIGHT_SEP = ","
  ! End of line character.
  character, parameter :: EOL = "/"
  ! Null value. This could be any non-positive integers.
  integer, parameter :: NULL_VALUE = -1
  ! Ignore value. This could also be any non-positive integers.
  integer, parameter :: IGNORE_VALUE = 0
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readScalarModelParamCfg
  !>  Read the scalar model parameters from an external file.
  ! -------------------------------------------------------------------------- !
  subroutine readScalarModelParamCfg()
    
    character(len=:), allocatable :: strippedFile
    character(len=:), allocatable :: key
    character(len=:), allocatable :: valChar
    character :: currChar
    logical   :: isReadingKey
    integer   :: charNum
    integer   :: fileStatus

    ! Read file.
    open(unit=MODEL_UNIT, file=FILE_NAME_MODEL, status='old', iostat=fileStatus)

    ! Warn missing file. TODO: Make better warning messages.
    if (fileStatus /= 0) then
      print "(a)", "***ERROR. Cannot read '", trim(FILE_NAME_MODEL),&
          "'."
      error stop
    end if

    ! Clean model config file.
    call stripFile(strippedFile, MODEL_UNIT)    
    close(MODEL_UNIT)

    ! Evaluate file.
    allocate(character(len=0) :: valChar)
    isReadingKey = .true.
    key = ""
    do charNum = 1, len(strippedFile)
      currChar = strippedFile(charNum:charNum)

      ! Check change in read state.
      evalChar: select case (currChar)
        ! ***KEYVAL_SEP: Shift from reading LHS to RHS.
        case (KEYVAL_SEP)
          isReadingKey = .false.
          valChar = ""
          cycle

        ! ***EOL: End of line character. 
        ! Assign value and proceed to the next line.
        case (EOL)
          isReadingKey = .true.
          call interpretKeyVal(key, valChar)

        ! ***Default case.
        case default
          ! 'Eat' input.
          if (isReadingKey) then
            key = key // currChar
          else
            valChar = valChar // currChar
          end if
      end select evalChar
    end do

    ! Check for unassigned values.
    call checkParamAssignedStatus()

    deallocate(strippedFile)
    deallocate(key)
    deallocate(valChar)
  end subroutine readScalarModelParamCfg


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: interpretKeyVal
  !>  Interpret and assign the key-value pairs with the corresponding model
  !!  parameter. 
  ! -------------------------------------------------------------------------- !
  subroutine interpretKeyVal(key, valChar)
    character(len=:), allocatable, intent(inout) :: key
    character(len=:), allocatable, intent(inout) :: valChar

    integer :: keyIdx
    
    ! Read the obtained key and value.
    keyIdx = getCharArrayIndex(key)
    select case (keyIdx)
      ! ***IGNORE_VALUE: The current line is just a line of comment or
      ! whitespaces.
      case (IGNORE_VALUE)
        ! Reset `key` for the next line to read.
        key = ""

      ! NULL_VALUE: Invalid key was passed.
      case (NULL_VALUE)
        print "(5a)", "***ERROR. '", key, "' in '", &
            trim(FILE_NAME_MODEL), "' is not a valid parameter."
        error stop

      ! ***Defaut case: The given key is valid.
      case default
        call assignValue(valChar, keyIdx)
        key = ""
    end select
  end subroutine interpretKeyVal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignValue
  !>  Assign parameter obtained from the config files.
  ! -------------------------------------------------------------------------- !
  subroutine assignValue(valChar, keyIdx)
    use CastProcedures, only: castCharToInt

    character(len=:), allocatable, intent(inout) :: valChar
    integer,                       intent(in)    :: keyIdx

    integer :: status

    status = 0
    select case (keyIdx)
      case (1)
        MODEL_L = castCharToInt(valChar, status)
      case (2)
        MODEL_T = castCharToInt(valChar, status)
      case (3)
        MODEL_B = castCharToInt(valChar, status)
      case (4)
        MODEL_M = castCharToInt(valChar, status)
      case (5)
        MODEL_R = castCharToInt(valChar, status)
      case (6)
        MODEL_R_MAX = castCharToInt(valChar, status)
      case (7)
        MODEL_K = castCharToInt(valChar, status)
      case (8)
        MODEL_START_POP_SIZE = castCharToInt(valChar, status)
      case (9)
        MODEL_TIME_STEPS = castCharToInt(valChar, status)
      case (10)
        PROG_SAMPLE_SIZE = castCharToInt(valChar, status)
      case (11)
        PROG_REC_FLAG = valChar
      case (12)
        PROG_RNG = castCharToInt(valChar, status)
      case (13)
        PROG_RNG_SEED = castCharToInt(valChar, status)
      case (14)
        MODEL_MTTN_COUNT = castCharToInt(valChar, status)
    end select

    if (status /= 0) then
      print "(*(a))", "***ERROR. The value assigned to '", &
          trim(PARAM_KEYS(keyIdx)), "' in ", &
          trim(FILE_NAME_MODEL), "is not valid."
      error stop
    end if

    PARAM_ASSIGNED(keyIdx) = .true.
  end subroutine assignValue
  

  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCharArrayIndex
  !>  Get the corresponding index of `elem` in a rank-1 array of characters 
  !!  `elem`. If `elem` is not found in `array`, it returns `NULL_VALUE`.
  ! -------------------------------------------------------------------------- !
  pure function getCharArrayIndex(elem) result(i)
    character(len=:), allocatable, intent(in) :: elem
      !! Character whose corresponding index is to be obtained.

    integer :: i

    do i = 1, PARAM_COUNT
      if (PARAM_KEYS(i) == elem) return
    end do

    if (elem == "") then
      i = IGNORE_VALUE
    else
      i = NULL_VALUE
    end if
  end function getCharArrayIndex


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkParamAssignedStatus
  !>  Check if all parameters are assigned or not.
  ! -------------------------------------------------------------------------- !
  subroutine checkParamAssignedStatus()
    integer :: i
    logical :: isFirstMissing

    if (.not. all(PARAM_ASSIGNED)) then
      write(*, "(3a)", advance="no") "***ERROR. In '", trim(FILE_NAME_MODEL), &
          "', the following parameters are absent: "
      
      ! Search for the missing parameters.
      isFirstMissing = .true.
      do i = 1, PARAM_COUNT
        if (.not. PARAM_ASSIGNED(i)) then
          ! Print delimiter.
          if (isFirstMissing) then
            isFirstMissing = .false.
          else
            write(*, "(a)", advance="no") ", "
          end if

          write(*, "(a)", advance="no") trim(PARAM_KEYS(i))
        end if
      end do

      ! Print new line.
      print *, ""
      error stop
    end if
  end subroutine checkParamAssignedStatus


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: stripFile
  !>  Read the specified file and strip whitespaces and
  !!  comments (lines starting with ';').
  ! -------------------------------------------------------------------------- !
  subroutine stripFile(strippedFile, unit)
    character(len=:), allocatable, intent(out) :: strippedFile
      !! Content of a file whose comments and extra white-spaces are stripped.
    integer,                       intent(in)  :: unit
      !! Unit integer corresponding to the file from which `strippedFile` is
      !! obtained.

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
    open(unit=VWEIGHT_UNIT, file=FILE_NAME_VWEIGHT, status='old', &
        iostat=fileStatus)

    ! Warn missing file. TODO: Make better warning messages.
    if (fileStatus /= 0) then
      print "(a/, 3(a))", "***", &
          "WARNING. Cannot read '", & 
          trim(FILE_NAME_VWEIGHT), &
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
            print "(a)", "***ERROR. Unknown error when reading Verhulst weights."
            error stop
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
  logical pure function isNumeric(char)
    character, intent(in) :: char
      !! Character to be compared.

    integer :: asciiNum
  
    ! NOTE: ASCII characters from 48 to 57 are the numbers 0 to 9.
    asciiNum = iachar(char)
    isNumeric = (48 <= asciiNum .and. asciiNum <= 57)
  end function isNumeric
end submodule

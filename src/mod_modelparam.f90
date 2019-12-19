module ModelParam
  ! -------------------------------------------------------------------------- !
  ! MODULE:  ModelParam
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing the Penna model parameters.
  !
  !   Parameters
  !   ----------
  !     L : integer
  !         Genome length.
  !     T : integer
  !         Mutation threshold.
  !     B : integer
  !         Birth rate.
  !     M : integer
  !         Number of mutations individuals incur at birth.
  !     R : integer
  !         The age at which individuals reproduce.
  !     R_max : integer
  !         The inclusive upper limit of age an individual
  !         can reproduce.
  !     N_start : integer
  !         Starting population count.
  !     K : integer
  !         Carrying capacity.
  !     Verhulst weight : array[real]
  !         Weights of the Verhulst factor per age.
  !         NOTE: Verhulst factor is defined as so
  !
  !               V_a = 1 - (N(t)/K)*w_a
  ! 
  !         where V_a  : Verhulst factor at age `a`.
  !               N(t) : Population size at time `t`.
  !               K    : The carrying capacity.
  !               w_a  : Verhulst weight at age `a`.
  ! -------------------------------------------------------------------------- !
  implicit none
  private

  ! Parameters whose values are from `model.ini`.
  integer, protected, public, save :: MODEL_L     ! Genome length (unmodifiable)
  integer, protected, public, save :: MODEL_T     ! Mutation threshold
  integer, protected, public, save :: MODEL_B     ! Birth rate
  integer, protected, public, save :: MODEL_M     ! Mutation rate
  integer, protected, public, save :: MODEL_R     ! Reproduction age
  integer, protected, public, save :: MODEL_R_MAX ! Maximum reproduction age
  integer, protected, public, save :: MODEL_K     ! Carrying capacity

  ! Parameters whose values are from `verhulst_weights.ini`.
  ! Verhulst weights.
  real, allocatable, protected, public, save :: MODEL_VERHULST_W(:)
  ! Default Verhulst weight.
  real, parameter :: VERHULST_W_DEFAULT = 0. 

  ! Parameters whose values can be changed by command line arguments.
  integer, protected, public, save :: MODEL_N0            ! Starting pop size
  integer, protected, public, save :: MODEL_TIME_STEPS    ! Total time steps
  integer, protected, public, save :: MODEL_SAMPLE_SIZE   ! Sample size
  integer, protected, public, save :: MODEL_REC_FLAG      ! Record flag

  ! -------------------------------------------------------------------------- !
  ! Buffer character length.
  integer, parameter  :: MAXLEN = 256
  ! Filenames from which model parameters are obtained.
  character(len=MAXLEN), protected, public :: modelFilename = &
      "config/model.cfg"
  character(len=MAXLEN), protected, public :: vWeightsFilename = &
      "config/v_weight.cfg"

  ! -------------------------------------------------------------------------- !
  ! Parameter count.
  integer, parameter :: MODEL_PARAM_COUNT = 11
  ! Parameter keys. NOTE: Padded with spaces to accept initializer.
  character(len=MAXLEN), parameter :: modelParamKeys(MODEL_PARAM_COUNT) = &
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
       "rec_flag   "]
  ! Default parameter values. NOTE: The order is the same with `modelParamKeys`.
  integer, parameter :: modelParamDefault(MODEL_PARAM_COUNT) = &
      [32, 3, 1, 1, 9, 9, 20000, 100, 100, 1, 0]

  ! -------------------------------------------------------------------------- !
  ! Units for writing on files.
  integer, parameter :: modelUnit = 99
  integer, parameter :: vWeightUnit = 98

  ! Comment character.
  character, parameter :: COMMENT = ";"
  ! Key-value separator.
  character, parameter :: KEYVAL_SEP = "="
  ! Verhulst weight separator.
  character, parameter :: VWEIGHT_SEP = ","
  ! End of line character.
  character, parameter :: EOL = "/"
  ! Default null value. This could be any non-positive integers.
  integer, parameter :: NULLVALUE = -1
  ! Ignore value. This could also be any non-positive integers.
  integer, parameter :: IGNOREVALUE = 0 

  public :: readScalarParam
  public :: readVerhulstWeights
  public :: deallocVerhulstWeights
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCharArrayIndex
  !>  Get the corresponding index of `elem` in a rank-1 array of 
  !!  characters `elem`. If `elem` is not found in `array`, it
  !!  returns `nullValue` which is set to 0.
  ! -------------------------------------------------------------------------- !
  function getCharArrayIndex(elem) result(i)
    implicit none
    character(len=:), allocatable, intent(in) :: elem

    integer :: i

    do i = 1, size(modelParamKeys)
      if (trim(modelParamKeys(i)) == elem) return
    end do

    if (elem == "") then
      i = IGNOREVALUE
    else
      i = NULLVALUE
    end if
  end function getCharArrayIndex


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignParameters
  !>  Assign model parameters from an array of integer `values`.
  ! -------------------------------------------------------------------------- !
  subroutine assignParameters(values)
    implicit none
    integer, intent(in) :: values(:)

    integer :: i

    do i = 1, MODEL_PARAM_COUNT
      select case (i)
        case (1)
          MODEL_L = values(i)
        case (2)
          MODEL_T = values(i)
        case (3)
          MODEL_B = values(i)
        case (4)
          MODEL_M = values(i)
        case (5)
          MODEL_R = values(i)
        case (6)
          MODEL_R_MAX = values(i)
        case (7)
          MODEL_K = values(i)
        case (8)
          MODEL_N0 = values(i)
        case (9)
          MODEL_TIME_STEPS = values(i)
        case (10)
          MODEL_SAMPLE_SIZE = values(i)
        case (11)
          MODEL_REC_FLAG = values(i)
      end select
    end do
  end subroutine assignParameters


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printModelParam
  !>  Print the model parameters for error/warning messages.
  ! -------------------------------------------------------------------------- !
  subroutine printModelParam()
    implicit none

    integer :: i

    do i = 1, MODEL_PARAM_COUNT
      print "(4(' '), a12, ': ', i0)", modelParamKeys(i), modelParamDefault(i)
    end do

    ! Print separator.
    print "(a)", "***"
  end subroutine printModelParam


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readScalarParams
  !>  Read the scalar model parameters from an external file.
  ! -------------------------------------------------------------------------- !
  subroutine readScalarParam
    implicit none
    
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
    values(:) = modelParamDefault

    ! Read file.
    open(unit=modelUnit, file=modelFilename, status='old', iostat=fileStatus)

    ! Warn missing file. TODO: Make better warning messages.
    if (fileStatus /= 0) then
      print "(a/, 3a)", "***", "WARNING. Cannot read '", trim(modelFilename), &
          "'. Using the following default values:"
      call printModelParam()
      call assignParameters(modelParamDefault)
      return
    end if

    ! Clean model config file.
    call stripFile(strippedFile, modelUnit)    
    close(modelUnit)

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
            case (IGNOREVALUE)
              key = ""
              cycle
            case (NULLVALUE)
              print "(3(a))", "***WARNING. '", key, &
                  "' is not a valid parameter. Ignoring this key."
              print "(a/, 10(' '), *(a, ', '))", "Valid parameters:", &
                  (trim(modelParamKeys(i)), i = 1, size(modelParamKeys))
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

    call assignParameters(values)
    deallocate(strippedFile)
    deallocate(key)
    deallocate(strVal)
  end subroutine readScalarParam


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readVerhulstWeights
  !>  Read the Verhulst weights values from a .ini file.
  ! -------------------------------------------------------------------------- !
  subroutine readVerhulstWeights
    implicit none

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
    MODEL_VERHULST_W(:) = VERHULST_W_DEFAULT

    ! Read file.
    allocate(character(len=0) :: strippedFile)
    open(unit=vWeightUnit, file=vWeightsFilename, status='old', &
        iostat=fileStatus)

    ! Warn missing file. TODO: Make better warning messages.
    if (fileStatus /= 0) then
      print "(a/, 3(a))", "***", &
          "WARNING. Cannot read '", & 
          trim(vWeightsFilename), &
          "'. Using the following default value:"
      print "(/a, i0, /a, f4.2)", "age:    1-", MODEL_L, "weight: ", &
          VERHULST_W_DEFAULT

      ! Print separator.
      print "(a)", "***"
      return
    end if
    
    ! Clean Verhulst weight config file. 
    call stripFile(strippedFile, vWeightUnit)
    close(vWeightUnit)

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
                "default value (", VERHULST_W_DEFAULT, ")."

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
              "Using the default value (", VERHULST_W_DEFAULT, ")."
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
  end subroutine readVerhulstWeights


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: stripFile
  !>  Read the specified file and strip whitespaces and
  !!  comments (lines starting with ';').
  ! -------------------------------------------------------------------------- !
  subroutine stripFile(strippedFile, unit)
    implicit none
    character(len=:), allocatable, intent(out) :: strippedFile
    integer,                       intent(in)  :: unit

    character(len=:), allocatable :: line
    character(len=MAXLEN)         :: rawLine
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
  end subroutine


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isNumeric
  !>  Check whether the character `char` is a number or not.
  ! -------------------------------------------------------------------------- !
  logical function isNumeric(char)
    implicit none
    character, intent(in) :: char

    integer :: asciiNum
  
    ! NOTE: ASCII characters from 48 to 57 are the numbers 0 to 9.
    asciiNum = iachar(char)
    isNumeric = (48 <= asciiNum .and. asciiNum <= 57)
  end function isNumeric


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocVerhulstWeights
  !>  Deallocate `MODEL_VERHULST_W` array.
  ! -------------------------------------------------------------------------- !
  subroutine deallocVerhulstWeights
    implicit none
  
    if (allocated(MODEL_VERHULST_W)) deallocate(MODEL_VERHULST_W)
  end subroutine deallocVerhulstWeights
end module ModelParam

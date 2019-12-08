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
  ! Filenames from which model parameters are obtained.
  integer, parameter  :: MAXLEN = 256
  character(len=MAXLEN), protected, public :: modelFilename = &
      "config/model.cfg"
  character(len=MAXLEN), protected, public :: vWeightsFilename = &
      "config/v_weight.cfg"

  ! -------------------------------------------------------------------------- !
  ! Parameter count.
  integer, parameter :: modelParamCount = 11
  ! Default parameter values. NOTE: The order of the model parameters:
  !       [L, T, B, M, R, R_max, K, N0, t_max, sample_size, rec_flag]
  integer, parameter :: modelParamDefault(modelParamCount) = &
      [32, 3, 1, 1, 9, 9, 20000, 100, 100, 1, 0]
  ! Parameter keys. NOTE: Must first be initialized before using to be able
  ! to contain keys with different lengths (not counting spaces).
  character(len=MAXLEN) :: modelParamKeys(modelParamCount)

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
  ! SUBROUTINE: initParamKeys
  !>  Initialize `modelParamKeys` to allow it to contain keys with
  !!  different lengths (not counting spaces). It is not standard
  !!  to initialize character arrays with characters of differing
  !!  lengths.
  ! -------------------------------------------------------------------------- !
  subroutine initParamKeys
    implicit none
    integer :: i
    do i = 1, size(modelParamKeys)
      select case (i)
        case (1)
          modelParamKeys(i) = "L"
        case (2)
          modelParamKeys(i) = "T"
        case (3)
          modelParamKeys(i) = "B"
        case (4)
          modelParamKeys(i) = "M"
        case (5)
          modelParamKeys(i) = "R"
        case (6)
          modelParamKeys(i) = "R_max"
        case (7)
          modelParamKeys(i) = "K"
        case (8)
          modelParamKeys(i) = "N0"
        case (9)
          modelParamKeys(i) = "t_max"
        case (10)
          modelParamKeys(i) = "sample_size"
        case (11)
          modelParamKeys(i) = "rec_flag"
      end select
    end do
  end subroutine initParamKeys


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
    integer             :: i

    do i = 1, modelParamCount
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
  ! SUBROUTINE: readScalarParams
  !>  Read the scalar model parameters from an external file.
  ! -------------------------------------------------------------------------- !
  subroutine readScalarParam
    implicit none
    
    integer :: values(modelParamCount)
    integer :: filestatus
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

    ! Initialize parameter keys.
    call initParamKeys

    ! Check whether the file exists or not.
    inquire(file=modelFilename, iostat=filestatus)
    if (filestatus /= 0) then
      print "(3a)", "***Cannot read '", modelFilename, &
          "'. Using the default values."
      return
    end if

    ! Read file.
    strippedFile = ""
    open(unit=modelUnit, file=modelFilename)
    call stripFile(strippedFile, modelUnit)
    close(modelUnit)

    ! Evaluate file.
    isReadingKey = .true.
    key = ""
    strVal = ""
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
              print "(3(a))", "***Warning. '", key, &
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
                print "(3(a))", "***Warning. '", strVal, "' is not a valid" // &
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
    
    ! Inquire file existence.
    inquire(file=vWeightsFilename, iostat=fileStatus)
    if (fileStatus /= 0) then
      print "(3a)", "***Cannot read '", vWeightsFilename, &
          "'. Using the default values."
      return
    end if

    ! Read file.
    strippedFile = ""
    open(unit=vWeightUnit, file=vWeightsFilename)
    call stripFile(strippedFile, vWeightUnit)
    close(vWeightUnit)

    ! Evaluate stripped input.
    vWeightStr = ""
    vWeightIdx = 1
    strippedFile = strippedFile // EOL
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

          ! ***Error. Invalid number of weights.
          else if (vWeightIdx > MODEL_L) then
            print "(a)", "***Warning. Given Verhulst weights exceeded the " // &
                "maximum number of allowed number of weights."

          ! ***Error. Invalid range.
          else if (vWeight > 1) then
            print "(a, f5.3, a)", "***Warning. Given Verhulst weight" // &
                "is outside the allowed range [0, 1]. Using the " // &
                "default value (", VERHULST_W_DEFAULT, ")."

          ! ***Unknown error.
          else
            stop "***Error. Unknown error when reading Verhulst weights."
          end if AssignWeight
          ! <<<
          ! ==============================================================

        ! ***Error. Invalid input.
        else
          print "(3(a), f5.3, a)", "***Warning. '", vWeightStr , &
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

    strippedFile = ""
    line = ""
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



module Flag
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Flag
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing `flags` for indicating an individual's state.
  ! -------------------------------------------------------------------------- !
  implicit none

  integer, parameter :: ALIVE = 1
  integer, parameter :: DEAD_OLD_AGE = 2
  integer, parameter :: DEAD_MUTATION = 3
  integer, parameter :: DEAD_VERHULST = 4
  integer, parameter :: DEATH_REASONS(4) = &
      [ALIVE,        &
      DEAD_OLD_AGE,  &
      DEAD_MUTATION, &
      DEAD_VERHULST]
end module Flag

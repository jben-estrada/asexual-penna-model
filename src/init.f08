module Model
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Model
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
  integer, public, save :: MODEL_L     ! Genome length (unmodifiable)
  integer, public, save :: MODEL_T     ! Mutation threshold
  integer, public, save :: MODEL_B     ! Birth rate
  integer, public, save :: MODEL_M     ! Mutation rate
  integer, public, save :: MODEL_R     ! Reproduction age
  integer, public, save :: MODEL_R_MAX ! Maximum reproduction age
  integer, public, save :: MODEL_K     ! Carrying capacity

  ! Parameters whose values are from `verhulst_weights.ini`.
  real, allocatable, public, save :: MODEL_VERHULST_W(:)  ! Verhulst weights
  real, parameter                 :: VERHULST_W_DEFAULT = 0. ! Default weight

  ! Parameters whose values can be changed by command line arguments.
  integer, public, save :: MODEL_N0            ! Starting pop size
  integer, public, save :: MODEL_TIME_STEPS    ! Total time steps
  integer, public, save :: MODEL_SAMPLE_SIZE   ! Sample size

  ! -------------------------------------------------------------------------- !
  ! Filenames from which model parameters are obtained.
  integer, parameter               :: MAXLEN = 256
  character(len=MAXLEN), protected :: modelFilename = "model.ini"
  character(len=MAXLEN), protected :: vWeightsFilename = "verhulst_weights.ini"

  ! -------------------------------------------------------------------------- !
  ! Parameters and their default values.
  integer, parameter :: modelParamCount = 10
  ! NOTE: The order of the model parameters:
  !       [L, T, B, M, R, R_max, K, N0, t_max, sample_size]
  integer, parameter :: modelParamDefault(modelParamCount) = &
      [32, 3, 1, 1, 9, 9, 20000, 100, 100, 1]

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
  ! Default null value. This could be anything.
  integer, parameter :: NULLVALUE = -1
  ! Ignore value.
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

    ! NOTE: Inelegant solution. It seems character arrays with differing
    ! character lengths are not allowed.
    i = NULLVALUE
    if (elem == "L")     i = 1
    if (elem == "T")     i = 2
    if (elem == "B")     i = 3
    if (elem == "M")     i = 4
    if (elem == "R")     i = 5
    if (elem == "R_max") i = 6
    if (elem == "K")     i = 7
    if (elem == "N0")    i = 8
    if (elem == "t_max") i = 9
    if (elem == "sample_size") i =10 
    if (elem == "")      i = IGNOREVALUE
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
      if (i == 1) MODEL_L = values(i)
      if (i == 2) MODEL_T = values(i)
      if (i == 3) MODEL_B = values(i)
      if (i == 4) MODEL_M = values(i)
      if (i == 5) MODEL_R = values(i)
      if (i == 6) MODEL_R_MAX = values(i)
      if (i == 7) MODEL_K = values(i)
      if (i == 8) MODEL_N0 = values(i)
      if (i == 9) MODEL_TIME_STEPS = values(i)
      if (i == 10) MODEL_SAMPLE_SIZE = values(i)
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

    ! Assign default values.
    values = modelParamDefault

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
                  "'is not a valid parameter. Ignoring this key."
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

      ! Check read state.
      if (currChar == VWEIGHT_SEP .or. charNum == len(strippedFile)) then
        read(vWeightStr, *, iostat=readStatus) vWeight

        ! Check if the char-to-real casting succeeds.
        if (readStatus == 0) then
          ! Check all possible errors before assigning.
          if (vWeightIdx <= MODEL_L .and. vWeight <= 1) then
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
          end if
        ! ***Error. Invalid input.
        else
          print "(3(a), f5.3, a)", "***Warning. '", vWeightStr , &
              "' is not a valid value for a Verhulst factor. " // &
              "Using the default value (", VERHULST_W_DEFAULT, ")."
        end if

        vWeightIdx = vWeightIdx + 1
        vWeightStr = ""
      ! Continue reading values for Verhulst weights.
      else
        if (isNumeric(currChar) .or. currChar == ".") &
            vWeightStr = vWeightStr // currChar
      end if
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
    character                     :: currChar
    integer                       :: readStatus
    integer                       :: charNum

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
    integer               :: asciiNum
  
    asciiNum = iachar(char)
    ! NOTE: ASCII characters from 48 to 57 are the numbers 0 to 9.
    if (48 <= asciiNum .and. asciiNum <= 57) then
      isNumeric = .true.
    else
      isNumeric = .false.
    end if
  end function isNumeric


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocVerhulstWeights
  !>  Deallocate `MODEL_VERHULST_W` array.
  ! -------------------------------------------------------------------------- !
  subroutine deallocVerhulstWeights
    implicit none
  
    if (allocated(MODEL_VERHULST_W)) deallocate(MODEL_VERHULST_W)
  end subroutine deallocVerhulstWeights
end module Model


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


module StdKind
  ! -------------------------------------------------------------------------- !
  ! MODULE:  StdKind
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing standard kinds for variables in related parts.
  ! -------------------------------------------------------------------------- !
  use iso_fortran_env, only: int64, int32, real64
  implicit none
  private

  integer, public, parameter :: personRealKind = real64
  integer, public, parameter :: personIntKind = int64

  integer, public, parameter :: timingRealKind = real64
  integer, public, parameter :: timingIntKind = int64

  integer, public, parameter :: writeRealKind = real64
  integer, public, parameter :: writeIntKind = int64

  integer, public, parameter :: arrayRealKind = real64
  integer, public, parameter :: arrayIntKind = int32
end module StdKind

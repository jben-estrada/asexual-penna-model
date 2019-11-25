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
  integer, public, save :: MODEL_N0 = 100          ! Starting pop size
  integer, public, save :: MODEL_TIME_STEPS = 100  ! Total time steps

  ! -------------------------------------------------------------------------- !
  ! Filenames from which model parameters are obtained.
  integer, parameter               :: MAXLEN = 80
  character(len=MAXLEN), protected :: modelFilename = "model.ini"
  character(len=MAXLEN), protected :: vWeightsFilename = "verhulst_weights.ini"

  ! -------------------------------------------------------------------------- !
  ! Parameters and their default values.
  integer, parameter :: modelParamCount = 9
  integer, parameter :: modelParamDefault(modelParamCount) = &
      [32,   3,   1,   1,   9,   9,   20000, 100, 100]

  ! -------------------------------------------------------------------------- !
  ! Units for writing on files.
  integer, parameter :: modelUnit = 99
  integer, parameter :: vWeightUnit = 98

  ! Stop character for reading files.
  character(len=MAXLEN), parameter :: endOfList = "//"
  ! Key-value seperator.
  character(len=1), parameter :: keyValSep = "="
  ! Default null value. This could be anything.
  integer, parameter :: NULLVALUE = -1

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
    integer :: readStatus
    integer :: lineNum
    integer :: charNum

    ! Input variables
    character(len=MAXLEN)         :: rawLine
    character(len=:), allocatable :: line
    character(len=:), allocatable :: key
    character(len=:), allocatable :: strVal
    integer                       :: val
    logical                       :: isReadingKey
    integer                       :: keyIdx

    ! Initialize `value`
    values(:) = modelParamDefault
    allocate(character(len=0) :: line)
    allocate(character(len=0) :: key)

    ! Check whether the file exists or not.
    inquire(file=modelFilename, iostat=filestatus)
    if (filestatus /= 0) then
      print "(3a)", "***Cannot read '", modelFilename, &
          "'. Using the default values."
      return
    end if

    ! Read file.
    open(unit=modelUnit, file=modelFilename)

    ! Read line.
    lineNum = 1
    do
      read(modelUnit, "(a)", iostat=readStatus) rawLine
      if (readStatus == 0) then
        line = trim(rawLine)
      else
        print "(a, i0)", "***Error. Cannot read line ", lineNum
        exit
      end if

      ! Exit condition.
      if (line == endOfList) exit

      ! Initialize variables for reading chars in line.
      key = ""
      strVal = ""
      isReadingKey = .true.
      do charNum = 1, len(line)
        ! Check non-literal characters.
        if (line(charNum:charNum) == keyValSep) then
          isReadingKey = .false.
          cycle
        else if (line(charNum:charNum) == " ") then
          cycle
        end if

        ! Check literal characters.
        if (isReadingKey) then
          key = key // line(charNum:charNum)
        else
          strVal = strVal // line(charNum:charNum)
        end if
      end do

      ! Check whether `key` is valid or not.
      keyIdx = getCharArrayIndex(key)
      if (keyIdx == NULLVALUE) then
        print "(3(a))", "***Warning. '", key, "' is not a valid parameter."
      else
        ! Get the corresponding value.
        read(strVal, *, iostat=readStatus) val
        if (readStatus == 0) then
          values(keyIdx) = val
        else
          print "(3(a))", "***Warning. '", strVal, "' is not a valid value."
        end if
      end if

      lineNum = lineNum + 1
    end do

    call assignParameters(values)
    close(modelUnit)
    deallocate(line)
    deallocate(key)
    deallocate(strVal)
  end subroutine readScalarParam


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readVerhulstWeights
  !>  Read the Verhulst weights values from a .ini file.
  ! -------------------------------------------------------------------------- !
  subroutine readVerhulstWeights
    implicit none

    integer :: filestatus
    integer :: readstatus
    integer :: i
    real    :: verhulstWeight

    integer, parameter        :: charBuffer = 256
    character(len=charBuffer) :: vWeight_str

    ! Initialize Verhulst weight array.
    if (.not.allocated(MODEL_VERHULST_W)) allocate(MODEL_VERHULST_W(MODEL_L))
    MODEL_VERHULST_W(:) = VERHULST_W_DEFAULT
    
    ! Inquire file existence.
    inquire(file=vWeightsFilename, iostat=filestatus)
    if (filestatus /= 0) then
      print "(3a)", "***Cannot read '", vWeightsFilename, &
          "'. Using the default values."
      return
    end if

    ! Read file
    open(unit=vWeightUnit, file=vWeightsFilename)
    do i = 1, MODEL_L
      read(vWeightUnit, *, iostat=readStatus) vWeight_str

      if (readStatus == 0) then
        read(vWeight_str, *) verhulstWeight
        MODEL_VERHULST_W(i) = verhulstWeight
      end if
    end do

    close(vWeightUnit)
  end subroutine readVerhulstWeights


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

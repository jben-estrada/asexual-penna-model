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

  integer, public, save :: MODEL_L = 32        ! Genome length (unmodifiable)
  integer, public, save :: MODEL_T = 3         ! Mutation threshold
  integer, public, save :: MODEL_B = 1         ! Birth rate
  integer, public, save :: MODEL_M = 1         ! Mutation rate
  integer, public, save :: MODEL_R = 9         ! Reproduction age
  integer, public, save :: MODEL_R_MAX = 9     ! Maximum reproduction age
  integer, public, save :: MODEL_K = 20000     ! Carrying capacity
  integer, public, save :: MODEL_N0 = 100      ! Starting pop size
  integer, public, save :: MODEL_TIME_STEPS = 100  ! Total time steps

  real, allocatable, public, save :: MODEL_VERHULST_W(:)  ! Verhulst weights
  ! -------------------------------------------------------------------------- !
  ! Filenames from which model parameters are obtained.
  integer, parameter            :: MAXLEN = 32
  character(len=MAXLEN), public :: modelFilename = "model.ini"
  character(len=MAXLEN), public :: vWeightsFilename = "verhulst_weights.ini"
  ! -------------------------------------------------------------------------- !
  integer, parameter :: modelParamCount = 7
  integer, parameter :: nullValue = -1

  character(len=MAXLEN), parameter :: extParamName(modelParamCount) = &
      ["L", "T", "B", "M", "R", "S", "K"]
  character(len=MAXLEN), parameter :: endOfList = "//"

  ! Units for writing on files.
  integer, parameter :: modelUnit = 99
  integer, parameter :: vWeightUnit = 98

  public :: readIni
  public :: readVerhulstWeights
  public :: deallocVerhulstWeights
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readIni
  !>  Read the model parameters from a .ini file.
  ! -------------------------------------------------------------------------- !
  subroutine readIni
    implicit none

    integer :: status
    integer :: readStatus
    integer :: i
    integer :: values(modelParamCount)
    integer :: tempValue

    character(len=MAXLEN) :: key

    ! Initialize `value`
    values(:) = nullValue

    ! Check whether the file exists or not.
    inquire(file=modelFilename, iostat=status)
    if (status /= 0) then
      print "(3a)", "***Cannot read '", modelFilename, &
          "'. Using the default values."
      return
    end if

    ! Read file
    open(unit=modelUnit, file=modelFilename)
    do i = 1, modelParamCount
      read(modelUnit, "(a2, i6)", iostat=readStatus) key, tempValue

      ! Case handling
      if (readStatus /= 0) then
        print "(a, i2)", "***Cannot read line. Ending at line ", i
        exit
      else if(key == endOfList) then
        print *, "***Reading ended prematurely"
        return
      else if(.not.any(extParamName == key)) then
        print "(a, a1, a)", "***Warning. '", key, &
            "' is not a valid parameter."
        cycle
      end if
      values(getCharArrayIndex(extParamName, key)) = tempValue
    end do

    call assignParameters(values)
    close(modelUnit)
  end subroutine readIni

  
  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readVerhulstWeights
  !>  Read the Verhulst weights values from a .ini file.
  ! -------------------------------------------------------------------------- !
  subroutine readVerhulstWeights
    implicit none
    ! TODO
  
    if (.not.allocated(MODEL_VERHULST_W)) allocate(MODEL_VERHULST_W(MODEL_L))
    MODEL_VERHULST_W(:) = 0.
  end subroutine readVerhulstWeights


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocVerhulstWeights
  !>  Deallocate `MODEL_VERHULST_W` array.
  ! -------------------------------------------------------------------------- !
  subroutine deallocVerhulstWeights
    implicit none
  
    if (allocated(MODEL_VERHULST_W)) deallocate(MODEL_VERHULST_W)
  end subroutine deallocVerhulstWeights


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignParameters
  !>  Assign model parameters from an array of integer `values`.
  ! -------------------------------------------------------------------------- !
  subroutine assignParameters(values)
    implicit none

    integer, intent(in) :: values(:)
    integer             :: i

    ! NOTE: I can't think of a more elegant solution to this.
    do i = 1, modelParamCount
      select case(i)
      case(1)
        MODEL_L = values(i)
      case(2)
        MODEL_T = values(i)
      case(3)
        MODEL_B = values(i)
      case(4)
        MODEL_M = values(i)
      case(5)
        MODEL_R = values(i)
      case(6)
        MODEL_R_MAX = values(i)
      case(7)
        MODEL_K = values(i)
      end select
    end do
  end subroutine assignParameters


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCharArrayIndex
  !>  Get the corresponding index of `elem` in a rank-1 array of 
  !!  characters `elem`. If `elem` is not found in `array`, it
  !!  returns `nullValue` which is set to 0.
  ! -------------------------------------------------------------------------- !
  function getCharArrayIndex(array, elem) result(i)
    implicit none

    character(len=MAXLEN), intent(in) :: array(:)
    character(len=MAXLEN), intent(in) :: elem

    integer :: i

    do i = 1, size(array)
      if (array(i) == elem) return
    end do
 
    i = nullValue  ! Default value
  end function getCharArrayIndex
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

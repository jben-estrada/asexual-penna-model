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
  !         Length of genome of each individual.
  !     T : integer
  !         Threshold for number of mutations.
  !     B : integer
  !         Number of births an individual can give.
  !     M : integer
  !         Number of mutations an individual will incur.
  !     R : integer
  !         The age at which individuals can reproduce.
  !     R_max : integer
  !         The inclusive upper limit age an individual can reproduce.
  !     N_start : integer
  !         Starting population count.
  !     K : integer
  !         Carrying capacity.
  !     Verhulst weight : array[real]
  !         Weights of the Verhulst factor per age. 
  !         Modified Verhulst factors become as so.
  !                         v_i = 1 - (N(t)/K)*w_i
  ! 
  !         where v_k  : Verhulst factor at age `i`.
  !               N(t) : Population size at time `t`.
  !               K    : The carrying capacity.
  !               w_k  : Verhulst weight at age `i`.
  !
  !  NOTE: Parameters with `_D` suffixes are default values
  ! -------------------------------------------------------------------------- !
  use iso_fortran_env, only: real64, int64
  implicit none
  integer, save :: MODEL_L = 32              ! Genome length (unmodifiable)
  integer, save :: MODEL_T = 3               ! Mutation threshold
  integer, save :: MODEL_B = 1               ! Birth rate
  integer, save :: MODEL_M = 1               ! Mutation rate
  integer, save :: MODEL_R = 9               ! Reproduction age
  integer, save :: MODEL_R_MAX = 9           ! Maximum reproduction age
  integer, save :: MODEL_K = 20000           ! Carrying capacity
  integer, save :: MODEL_N0_D = 100          ! Default starting pop size
  integer, save :: MODEL_TIME_STEPS_D = 100  ! Default total time steps
  real, allocatable, save :: MODEL_VERHULST_W(:)  ! Verhulst weights
  ! -------------------------------------------------------------------------- !
  ! Standard integer and real kinds.
  ! NOTE: This does not apply for counter, timing and throwaway variables.
  !       This is solely for variables related to gene and genomes.
  integer, parameter :: stdIntKind = int64
  integer, parameter :: stdRealKind = real64
  ! -------------------------------------------------------------------------- !

  integer, private, parameter :: modelParamCount = 7
  integer, private, parameter :: MAXLEN = 32
  integer, private, parameter :: nullValue = -1
  character(len=MAXLEN), private, parameter :: extParamName(modelParamCount) = &
      ["L", "T", "B", "M", "R", "S", "K"]
  character(len=MAXLEN), private, parameter :: endOfList = "//"

  character(len=MAXLEN) :: modelFilename = "model.ini"
  character(len=MAXLEN) :: vWeightsFilename = "verhulst_weights.ini"
  integer, private, parameter :: modelUnit = 99
  integer, private, parameter :: vWeightUnit = 98

  private :: assignParameters
  private :: getCharArrayIndex
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
    integer :: i

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
  !>  Get the corresponding index of `elem` in a rank-1 array of characters
  !   `elem`. If `elem` is not found in `array`, it returns `nullValue` which
  !   is set to 0.
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

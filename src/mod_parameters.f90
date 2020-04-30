module Parameters
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Parameters
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing the Penna model parameters and other common data.
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
  !     init_mttn_count: integer
  !         Initial mutation count per individual.
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

  ! EXTRANEOUS VARIABLES
  ! -------------------------------------------------------------------------- !
  integer, parameter :: MAX_LEN = 256
    !! Maximum character length for buffer characters.
  integer, parameter :: VOID_INT = -1
    !! Placeholder integer value.

  ! Print states.
  integer, public, parameter :: NORMAL_PRINT = 0
    !! Flag corresponding to default printing of model parameters.
  integer, public, parameter :: VERBOSE_PRINT = 1
    !! Flag corresponding to printing of all scalar model parameters.
  integer, public, parameter :: SILENT_PRINT = 2
    !! Flag corresponding to supressed printing.
  integer, public, parameter :: VERSION_PRINT = 3
    !! Flag corresponding to printing of the program version.

  ! PROGRAM DETAILS
  ! -------------------------------------------------------------------------- !
  character(len=*), public, parameter :: PROG_NAME = &
      "Asexual Penna model simulation"
    !! Name of the program
  character(len=*), public, parameter :: PROG_DESC = &
      "A simulation for the 'Penna model', a biological aging model."
    !! Description of the program
  character(len=*), public, parameter :: PROG_VERSION = ""
    !! Program version. 
    !! NOTE: Temporarily set to none as I still have to work out the appropriate
    !! versioning scheme.

  integer, public, target, protected :: PROG_PRINT_STATE = NORMAL_PRINT
    !! Printing state. 
  logical, public,         protected :: PROG_RECORD_TIME = .false.
    !! Record-time state.
  integer, public, target, protected :: PROG_SAMPLE_SIZE = VOID_INT
    !! Number of times the simulation will run.
    !! NOTE: Default value is obtained from config files.
  integer, public, target, protected :: PROG_RNG = VOID_INT
    !! RNG flag. Corresponds to a random number generator.
    !! NOTE: Default value is obtained from config files.
  integer, public, target, protected :: PROG_RNG_SEED = VOID_INT
    !! RNG seed.
    !! NOTE: Default value is obtained from config files.
  character(len=MAX_LEN), public, target, protected :: PROG_REC_FLAG
    !! List of record flags.
    !! NOTE: Default value is obtained from config files.
  character(len=MAX_LEN), public, target, protected :: PROG_OUT_FILE_NAME = &
      "./out.csv"
    !! Name of the file to which output of data to be recorded is to be written.

  ! MODEL PARAMETERS
  ! -------------------------------------------------------------------------- !
  ! Parameters whose values are from an external config file.
  ! NOTE: Default values are obtained from config files.
  integer, public, target, protected :: MODEL_L = VOID_INT
    !! Genome length
  integer, public, target, protected :: MODEL_T = VOID_INT
    !! Mutation threshold
  integer, public, target, protected :: MODEL_B = VOID_INT
    !! Birth rate
  integer, public, target, protected :: MODEL_M = VOID_INT
    !! Mutation rate
  integer, public, target, protected :: MODEL_R = VOID_INT
    !! Reproduction age
  integer, public, target, protected :: MODEL_R_MAX = VOID_INT
    !! Maximum reproduction age
  integer, public, target, protected :: MODEL_K = VOID_INT
    !! Carrying capacity
  integer, public, target, protected :: MODEL_START_POP_SIZE = VOID_INT
    !! Starting pop size
  integer, public, target, protected :: MODEL_TIME_STEPS = VOID_INT
    !! Total time steps
  integer, public, target, protected :: MODEL_MTTN_COUNT = VOID_INT
    !! Initial mutation count of individuals in starting pop.

  real, allocatable, protected, public :: MODEL_V_WEIGHT(:)
    !! Verhulst weights.
  real, parameter :: VWEIGHT_DEFAULT = 0.
    !! Default Verhulst weight.

  ! PARAMETER LISTING FILE PATHS
  ! -------------------------------------------------------------------------- !
  ! Filenames from which model parameters are obtained.
  character(len=MAX_LEN), protected, target, public :: FILE_PARAM_LIST = &
      "model.cfg" !! Path for file containing scalar model parameters.

  ! SUBMODULE INTERFACE
  ! -------------------------------------------------------------------------- !
  ! Config read subroutines.
  interface
    module subroutine readDefaultParamVal()
    end subroutine
  end interface

  ! Public subroutines.
  interface
    module subroutine setParams()
    end subroutine

    module subroutine printProgDetails()
    end subroutine

    module subroutine deallocVerhulstWeights()
    end subroutine
  end interface
  ! -------------------------------------------------------------------------- !


  ! Initialization routines.
  public :: setParams

  ! Routine for memory management.
  public :: deallocVerhulstWeights
  ! Other routines.
  public :: printProgDetails
end module Parameters

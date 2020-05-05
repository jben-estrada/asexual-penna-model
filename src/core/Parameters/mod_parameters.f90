module Parameters
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Parameters
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
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
  use CmdArgParserType, only: &
    CmdArgParser, &
    FLAG_S => CMD_TYPE_FLAG_S, &
    FLAG_L => CMD_TYPE_FLAG_L, &
    KV_S => CMD_TYPE_KEYVAL_S, &
    KV_L => CMD_TYPE_KEYVAL_L, &
    FLAG_TOGGLED
  use ParamFileParserType, only: ParamFileParser
  use ErrorMSG, only: raiseError, raiseWarning
  use CastProcs, only: castCharToInt
  implicit none
  private

  ! EXTRANEOUS VARIABLES
  ! -------------------------------------------------------------------------- !
  integer, parameter :: MAX_LEN = 256
    !! Maximum character length for buffer characters.
  integer, parameter :: VOID_INT = -1
    !! Placeholder integer value.

  ! Print states.
  integer, parameter :: NORMAL_PRINT = 0
    !! Flag corresponding to default printing of model parameters.
  integer, parameter :: VERBOSE_PRINT = 1
    !! Flag corresponding to printing of all scalar model parameters.
  integer, parameter :: SILENT_PRINT = 2
    !! Flag corresponding to supressed printing.
  integer, parameter :: VERSION_PRINT = 3
    !! Flag corresponding to printing of the program version.

  ! Record flags.
  character, parameter :: REC_NULL = "x"
    !! Nothing (do not record).
  character, parameter :: REC_POP = "p"
    !! Population size per time step.
  character, parameter :: REC_AGE_DSTRB = "a"
    !! Age distribution in the last 300 time steps
  character, parameter :: REC_DEATH = "d"
    !! Death counts (death by age, by mutation, by Verhulst factor) 
    !! per time step.
  character, parameter :: REC_DIV_IDX = "s"
    !! Shannon diversity index per time step.
  character, parameter :: REC_GENE_DSTRB = "b"
    !! Bad gene distribution per time step.
  character, parameter :: REC_TIME = "t"
    !! Timing statistics.

  ! PROGRAM PARAMETERS
  ! -------------------------------------------------------------------------- !
  character(len=*), parameter :: PROG_NAME = &
      "Asexual Penna model simulation"
    !! Name of the program
  character(len=*), parameter :: PROG_DESC = &
      "A simulation for the 'Penna model', a biological aging model."
    !! Description of the program
  character(len=*), parameter :: PROG_VERSION = ""
    !! Program version. 
    !! NOTE: Temporarily set to none as I still have to work out the appropriate
    !! versioning scheme.

  integer, target, protected :: PROG_PRINT_STATE = NORMAL_PRINT
    !! Printing state. 
  logical,         protected :: PROG_RECORD_TIME = .false.
    !! Record-time state.
  integer, target, protected :: PROG_SAMPLE_SIZE = VOID_INT
    !! Number of times the simulation will run.
    !! NOTE: Default value is obtained from config files.
  integer, target, protected :: PROG_RNG = VOID_INT
    !! RNG flag. Corresponds to a random number generator.
    !! NOTE: Default value is obtained from config files.
  integer, target, protected :: PROG_RNG_SEED = VOID_INT
    !! RNG seed.
    !! NOTE: Default value is obtained from config files.
  character(len=MAX_LEN), target, protected :: PROG_REC_FLAG
    !! List of record flags.
    !! NOTE: Default value is obtained from config files.
  character(len=MAX_LEN), target, protected :: PROG_OUT_FILE_NAME = "./out.csv"
    !! Name of the file to which output of data to be recorded is to be written.
  character(len=MAX_LEN),         parameter :: PROG_TIME_FILE_NAME ="./time.csv"
    !! Name of the file to which timing statistics is to be written on.

  ! MODEL PARAMETERS
  ! -------------------------------------------------------------------------- !
  ! Parameters whose values are from an external config file.
  ! NOTE: Default values are obtained from config files.
  integer, target, protected :: MODEL_L = VOID_INT
    !! Genome length
  integer, target, protected :: MODEL_T = VOID_INT
    !! Mutation threshold
  integer, target, protected :: MODEL_B = VOID_INT
    !! Birth rate
  integer, target, protected :: MODEL_M = VOID_INT
    !! Mutation rate
  integer, target, protected :: MODEL_R = VOID_INT
    !! Reproduction age
  integer, target, protected :: MODEL_R_MAX = VOID_INT
    !! Maximum reproduction age
  integer, target, protected :: MODEL_K = VOID_INT
    !! Carrying capacity
  integer, target, protected :: MODEL_START_POP_SIZE = VOID_INT
    !! Starting pop size
  integer, target, protected :: MODEL_TIME_STEPS = VOID_INT
    !! Total time steps
  integer, target, protected :: MODEL_MTTN_COUNT = VOID_INT
    !! Initial mutation count of individuals in starting pop.

  real, allocatable, protected :: MODEL_V_WEIGHT(:)
    !! Verhulst weights.
  real, parameter :: VWEIGHT_DEFAULT = 0.
    !! Default Verhulst weight.

  ! PARAMETER LISTING FILE PATHS
  ! -------------------------------------------------------------------------- !
  ! Filenames from which model parameters are obtained.
  character(len=MAX_LEN), protected, target :: FILE_PARAM_LIST = "model.cfg"
    !! Path for file containing scalar model parameters.

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
      !! Assign parameters from external file and command arguments.
    end subroutine

    module subroutine printProgDetails()
      !! Pretty print the model parameters. Can print verbosely or print nothing
      !! if need be.
    end subroutine

    module subroutine freeParamAlloctbls()
      !! Free all allocatable objects and objects with allocatable attributes in
      !! the 'Parameter' module.
    end subroutine
  end interface

  ! -------------------------------------------------------------------------- !
  ! Program parameters.
  public :: PROG_NAME
  public :: PROG_DESC
  public :: PROG_VERSION
  public :: PROG_PRINT_STATE
  public :: PROG_RECORD_TIME
  public :: PROG_SAMPLE_SIZE
  public :: PROG_RNG
  public :: PROG_RNG_SEED
  public :: PROG_REC_FLAG
  public :: PROG_OUT_FILE_NAME
  public :: PROG_TIME_FILE_NAME
  
  ! Values for `PROG_PRINT_STATE`.
  public :: NORMAL_PRINT
  public :: VERBOSE_PRINT
  public :: SILENT_PRINT
  public :: VERSION_PRINT

  ! Values for `PROG_REC_FLAG`.
  public :: REC_NULL
  public :: REC_POP
  public :: REC_AGE_DSTRB
  public :: REC_DEATH
  public :: REC_DIV_IDX
  public :: REC_GENE_DSTRB
  public :: REC_TIME

  ! Model parameters.
  public :: MODEL_L
  public :: MODEL_T
  public :: MODEL_B
  public :: MODEL_M
  public :: MODEL_R
  public :: MODEL_R_MAX
  public :: MODEL_K
  public :: MODEL_START_POP_SIZE
  public :: MODEL_TIME_STEPS
  public :: MODEL_MTTN_COUNT
  public :: MODEL_V_WEIGHT

  ! Parameter listing file.
  public :: FILE_PARAM_LIST

  ! Initialization routines.
  public :: setParams

  ! Routine for memory management.
  public :: freeParamAlloctbls
  ! Other routines.
  public :: printProgDetails
end module Parameters

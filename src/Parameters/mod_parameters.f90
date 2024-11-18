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
  use CmdArgParserType, only:  &
    CmdArgParser,              &
    init_CmdArgParser,         &
    FLAG_S => CMD_TYPE_FLAG_S, &
    FLAG_L => CMD_TYPE_FLAG_L, &
    KV_S => CMD_TYPE_KEYVAL_S, &
    KV_L => CMD_TYPE_KEYVAL_L, &
    FLAG_TOGGLED
  use ParamFileParserType, only: ParamFileParser, init_ParamFileParser
  use ErrorMSG, only: raiseError, raiseWarning
  use CastProcs, only: castCharToInt, castCharToReal, castIntToChar, isFinite
  use, intrinsic :: iso_fortran_env, only: compiler_version, output_unit
  implicit none
  private

  ! EXTRANEOUS VARIABLES
  ! -------------------------------------------------------------------------- !
  integer, parameter :: MAX_LEN = 256
    !! Maximum character length for buffer characters.
  integer, parameter :: VOID_INT = -1
    !! Placeholder integer value.
  real,    parameter :: VOID_REAL = tiny(1.0)
    !! Placeholder real value.

  ! Print states.
  integer, parameter :: NORMAL_PRINT = 0
    !! Flag corresponding to default printing of model parameters.
  integer, parameter :: VERBOSE_PRINT = 1
    !! Flag corresponding to printing of all scalar model parameters.
  integer, parameter :: SILENT_PRINT = 2
    !! Flag corresponding to supressed printing.
  integer, parameter :: VERSION_PRINT = 3
    !! Flag corresponding to printing of the program version.


  ! PROGRAM PARAMETERS
  ! -------------------------------------------------------------------------- !
  character(len=*), parameter :: PROG_NAME = &
      "Asexual Penna model simulation"
    !! Name of the program
  character(len=*), parameter :: PROG_DESC = &
      "A simulation for the 'Penna model', a biological aging model."
    !! Description of the program
  character(len=*), parameter :: PROG_VERSION = "v0.3.0"
    !! Program version.

  integer, target, protected :: PROG_PRINT_STATE = NORMAL_PRINT
    !! Printing state.
  integer, target, protected :: PROG_SAMPLE_SIZE = VOID_INT
    !! Number of times the simulation will run.
    !! NOTE: Default value is obtained from config files.
  integer, target, protected :: PROG_RNG = VOID_INT
    !! RNG flag. Corresponds to a random number generator.
    !! NOTE: Default value is obtained from config files.
  integer, target, protected :: PROG_RNG_SEED = VOID_INT
    !! RNG seed.
    !! NOTE: Default value is obtained from config files.
  logical, target, protected :: PROG_IN_CSV_FMT = .false.
    !! Output files in CSV format.
  character(len=MAX_LEN), target, protected :: PROG_REC_FLAG
    !! List of record flags.
    !! NOTE: Default value is obtained from config files.
  character(len=MAX_LEN), target, protected :: PROG_OUT_FILE_NAME = "./out.csv"
    !! Name of the file to which output of data to be recorded is to be written.

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
  real,    target, protected :: MODEL_ENTROPY_ORDER = VOID_REAL
    !! Renyi entropy order value  
  integer, target, protected :: MODEL_AGE_DSTRB_INIT_TIMESTEP
    !! The intial time step till the final time step where the age distribution
    !! is taken.

  real, allocatable, protected :: MODEL_V_WEIGHT(:)
    !! Verhulst weights.
  real, parameter :: VWEIGHT_DEFAULT = 0.
    !! Default Verhulst weight.

  logical, allocatable, protected :: MODEL_GENOME_MASK(:)
    !! Genome mask. NOTE: TRUE is a masking value while FALSE is non-masking.
  logical, parameter :: GENOME_MASK_DEFAULT = .false.
    !! Default value for the genome mask element.
  integer, parameter :: GENOME_MASKING_INT    = 1
    !! The corresponding integer value for the masking value TRUE.
  integer, parameter :: GENOME_NONMASKING_INT = 0
    !! The corresponding integer value for the non-masking value FALSE.

  ! RECORD FLAGS
  ! -------------------------------------------------------------------------- !
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
    !! Genetic diversity index per time step (Normalized Shannon index)
  character, parameter :: REC_GENE_DSTRB = "b"
    !! Bad gene distribution per time step.
  character, parameter :: REC_TIME = "t"
    !! (Average) elapsed time and standard deviation if applicable.
  character, parameter :: REC_GNM_COUNT = "c"
    !! Number of unique genomes per time step.
  character(len=*), parameter :: REC_FLAG_PENNA = &
      REC_POP // REC_AGE_DSTRB // REC_DEATH // REC_DIV_IDX // REC_GENE_DSTRB //&
      REC_GNM_COUNT
    !! Record flags for Penna data.
  character(len=*), parameter :: REC_FLAG_PROG = REC_TIME
    !! Record flags for program run data.
  character(len=*), parameter :: REC_FLAG_ORDER = &
      REC_FLAG_PENNA // REC_FLAG_PROG
    !! Record flags and their respective order as position in the string.


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
  public :: PROG_SAMPLE_SIZE
  public :: PROG_RNG
  public :: PROG_RNG_SEED
  public :: PROG_REC_FLAG
  public :: PROG_IN_CSV_FMT
  public :: PROG_OUT_FILE_NAME
  
  ! Values for `PROG_PRINT_STATE`.
  public :: NORMAL_PRINT
  public :: VERBOSE_PRINT
  public :: SILENT_PRINT
  public :: VERSION_PRINT

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
  public :: MODEL_ENTROPY_ORDER
  public :: MODEL_V_WEIGHT
  public :: MODEL_GENOME_MASK
  public :: MODEL_AGE_DSTRB_INIT_TIMESTEP

  ! Record flags
  public :: REC_NULL
  public :: REC_POP
  public :: REC_AGE_DSTRB
  public :: REC_DEATH
  public :: REC_DIV_IDX
  public :: REC_GENE_DSTRB
  public :: REC_GNM_COUNT
  public :: REC_TIME
  public :: REC_FLAG_PENNA
  public :: REC_FLAG_PROG
  public :: REC_FLAG_ORDER

  ! Parameter listing file.
  public :: FILE_PARAM_LIST

  ! Initialization routines.
  public :: setParams

  ! Routine for memory management.
  public :: freeParamAlloctbls
  ! Other routines.
  public :: printProgDetails
end module Parameters

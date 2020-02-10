module ModelParam
  ! -------------------------------------------------------------------------- !
  ! MODULE:  ModelParam
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
  character(len=*), public, parameter :: PROG_VERSION = ""
    !! Program version. 
    !! NOTE: Temporarily set to none as I still have to work out the appropriate
    !! versioning scheme.

  integer, public, protected :: PROG_PRINT_STATE = NORMAL_PRINT
    !! Printing state. 
  logical, public, protected :: PROG_RECORD_TIME = .false.
    !! Record-time state.
  integer, public, protected :: PROG_SAMPLE_SIZE
    !! Number of times the simulation will run.
    !! NOTE: Default value is obtained from config files.
  integer, public, protected :: PROG_RNG
    !! RNG flag. Corresponds to a random number generator.
    !! NOTE: Default value is obtained from config files.
  integer, public, protected :: PROG_RNG_SEED
    !! RNG seed.
    !! NOTE: Default value is obtained from config files.
  character(len=MAX_LEN), public, protected :: PROG_REC_FLAG
    !! List of record flags.
    !! NOTE: Default value is obtained from config files.
  character(len=MAX_LEN), public, protected :: PROG_OUT_FILE_NAME = "./out.txt"
    !! Name of the file to which output of data to be recorded is to be written.

  ! MODEL PARAMETERS
  ! -------------------------------------------------------------------------- !
  ! Parameters whose values are from an external config file.
  ! NOTE: Default values are obtained from config files.
  integer, public, protected :: MODEL_L
    !! Genome length
  integer, public, protected :: MODEL_T
    !! Mutation threshold
  integer, public, protected :: MODEL_B
    !! Birth rate
  integer, public, protected :: MODEL_M
    !! Mutation rate
  integer, public, protected :: MODEL_R
    !! Reproduction age
  integer, public, protected :: MODEL_R_MAX
    !! Maximum reproduction age
  integer, public, protected :: MODEL_K
    !! Carrying capacity
  integer, public, protected :: MODEL_START_POP_SIZE
    !! Starting pop size
  integer, public, protected :: MODEL_TIME_STEPS
    !! Total time steps
  integer, public, protected :: MODEL_MTTN_COUNT
    !! Initial mutation count of individuals in starting pop.

  real, allocatable, protected, public :: MODEL_VERHULST_W(:)
    !! Verhulst weights.
  real, parameter :: VWEIGHT_DEFAULT = 0.
    !! Default Verhulst weight.

  ! CONFIGURATION FILE PATHS
  ! -------------------------------------------------------------------------- !
  ! Filenames from which model parameters are obtained.
  character(len=MAX_LEN), protected, public :: FILE_NAME_MODEL = &
      "model.cfg" !! Path for file containing scalar model parameters.
  character(len=MAX_LEN), protected, public :: FILE_NAME_VWEIGHT = &
      "v_weight.cfg" !! Path for file with Verhulst weights.


  ! SUBMODULE INTERFACE
  ! -------------------------------------------------------------------------- !
  ! Config read subroutines.
  interface
    module subroutine readScalarModelParamCfg()
    end subroutine

    module subroutine readVerhulstWeightsCfg()
    end subroutine
  end interface

  ! Public subroutines.
  interface
    module subroutine assignModelParams()
    end subroutine

    module subroutine assignConfigFilePaths()
    end subroutine

    module subroutine printProgDetails()
    end subroutine

    module subroutine deallocVerhulstWeights()
    end subroutine
  end interface
  ! -------------------------------------------------------------------------- !


  ! Initialization routines.
  public :: assignModelParams
  public :: assignConfigFilePaths

  ! Routine for memory management.
  public :: deallocVerhulstWeights
  ! Other routines.
  public :: printProgDetails
end module ModelParam

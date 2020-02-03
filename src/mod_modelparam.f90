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
  use CmdOptions
  implicit none
  private

  ! PROGRAM DETAILS
  ! -------------------------------------------------------------------------- !
  character(len=*), public, parameter :: PROG_NAME = &
      "Asexual Penna model simulation"
  character(len=*), public, parameter :: PROG_VERSION = ""

    ! Print states.
  integer, public, parameter :: NORMAL_PRINT = 0
    !! Flag corresponding to default printing of model parameters.
  integer, public, parameter :: VERBOSE_PRINT = 1
    !! Flag corresponding to printing of all scalar model parameters.
  integer, public, parameter :: SILENT_PRINT = 2
    !! Flag corresponding to supressed printing.
  integer, public, parameter :: VERSION_PRINT = 3
    !! Flag corresponding to printing of the program version.
  integer, public, protected :: PROG_PRINT_STATE = NORMAL_PRINT
    !! Printing state. 

  logical, public, protected :: PROG_RECORD_TIME = .false.
    !! Record-time state.

  ! MODEL PARAMETERS
  ! -------------------------------------------------------------------------- !
  integer, target :: modelParams(14) = 0
    !! Array of model parameters.

  integer, parameter :: MODEL_PARAM_COUNT = size(modelParams)
    !! Number of model parameters.

  ! Parameters whose values are from an external config file.
  integer, protected, pointer, public :: MODEL_L => &
    modelParams(1) !! Genome length
  integer, protected, pointer, public :: MODEL_T => &
    modelParams(2) !! Mutation threshold
  integer, protected, pointer, public :: MODEL_B => &
    modelParams(3) !! Birth rate
  integer, protected, pointer, public :: MODEL_M => &
    modelParams(4) !! Mutation rate
  integer, protected, pointer, public :: MODEL_R => &
    modelParams(5) !! Reproduction age
  integer, protected, pointer, public :: MODEL_R_MAX => &
    modelParams(6) !! Maximum reproduction age
  integer, protected, pointer, public :: MODEL_K => &
    modelParams(7) !! Carrying capacity

  real, allocatable, protected, public :: MODEL_VERHULST_W(:)
    !! Verhulst weights.
  real, parameter :: VWEIGHT_DEFAULT = 0.
    !! Default Verhulst weight.

  ! Parameters whose values can be changed by command line arguments.
  integer, protected, pointer, public :: MODEL_N0 => &
    modelParams(8)   !! Starting pop size
  integer, protected, pointer, public :: MODEL_TIME_STEPS => &
    modelParams(9)   !! Total time steps
  integer, protected, pointer, public :: MODEL_SAMPLE_SIZE => &
    modelParams(10)  !! Sample size
  integer, protected, pointer, public :: MODEL_REC_FLAG => &
    modelParams(11)  !! Record flag. Corresponds to data to be recorded.
  integer, protected, pointer, public :: MODEL_RNG => &
    modelParams(12)  !! RNG flag. Corresponds to a random number generator.
  integer, protected, pointer, public :: MODEL_RNG_SEED => &
    modelParams(13)  !! RNG seed.
  integer, protected, pointer, public :: MODEL_MTTN_COUNT => &
    modelParams(14)  !! Initial mutation count of individuals in starting pop.

  ! CONFIGURATION FILE PATHS
  ! -------------------------------------------------------------------------- !
  ! Buffer character length.
  integer, parameter :: MAX_LEN = 256
    !! Maximum character length for buffer characters.

  ! Filenames from which model parameters are obtained.
  character(len=MAX_LEN), protected, public :: FILE_NAME_MODEL = &
      "./bin/model.cfg" !! Path for file containing scalar model parameters.
  character(len=MAX_LEN), protected, public :: FILE_NAME_VWEIGHT = &
      "./bin/v_weight.cfg" !! Path for file with Verhulst weights.


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

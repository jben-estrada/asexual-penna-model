!------------------------------------------------------------------------------
! PROGRAM: Asexual Penna model
!------------------------------------------------------------------------------
!
!> @author
!> John Benedick A. Estrada
!
! DESCRIPTION: 
!>  Implementation of the asexual Penna model based on the
!!  description of S. Oliveira [1]. The difference between her
!!  implementation of the model and mine is that the model used
!!  here generalizes the Verhulst factor by allowing it to change
!!  with the age of the individuals.
!!
!!  Reference:
!!  [1]  S. Oliveira. "Evolution, ageing and speciation: Monte Carlo
!!       simulations of biological systems", In: Brazilian Journal of
!!       Physics 34.3B (2004), pp. 1066-1076
!
! MAJOR REVISION:
!   11-Nov-2019 - First complete version of the program.
!   23-Nov-2019 - Major change to population handling.
!   06-Dec-2019 - Enhanced the population handling (via OO approach).
!------------------------------------------------------------------------------


program Main
  use Penna
  use ModelParam
  use RNG, only: initializeRNG
  use CmdOptions, only: initializeCmdOptions, parseCmdArgs, showHelpMsgAndNotes
  implicit none

  ! Initialize the command-line options.
  call initializeCmdOptions()

  ! Get the paths of .cfg  files.
  ! -------------------------------------------------------------------------- !
  ! Initialize command-line optional arguments.
  call assignOptionalCfgFilePath()

  ! Get only the positional arguments.
  call parseCmdArgs(.false., .false., .true.)
  ! Assign the paths of .cfg files containing model parameters and
  ! Verhulst weights.
  call assignConfigFilePaths()
  ! -------------------------------------------------------------------------- !

  ! Get the model parameters from the .cfg files.
  ! -------------------------------------------------------------------------- !
  ! Read the .cfg files.
  call readScalarParam()
  call readVerhulstWeights()

  ! Assign the model parameters from the .cfg file as the default value.
  ! (Non-default values would come from command-line arguments.)
  call assignOptionalModelParamVal()

  ! Get the remaining command-line arguments.
  call parseCmdArgs(.true., .true., .false.)

  ! Finally, assign model parameters obtained from the command-line arguments.
  call assignModelParamFromCmdArgs()
  ! -------------------------------------------------------------------------- !

  ! -------------------------------------------------------------------------- !
  ! Initialize the RNG with the provided command-line arguments.
  call initializeRNG()

  ! Print the help message and stop the program if '-h' or '--help' is passed.
  call showHelpMsgAndNotes()

  ! Print the welcome text.
  call prettyPrintModelParams()
  ! -------------------------------------------------------------------------- !

  ! -------------------------------------------------------------------------- !
  ! Run the Penna model simulation.
  call run(MODEL_TIME_STEPS, MODEL_N0, MODEL_SAMPLE_SIZE, MODEL_REC_FLAG, &
      RECORD_TIME, PRINT_STATE /= SILENT_PRINT)
  ! -------------------------------------------------------------------------- !

  ! Wrap up.
  call deallocVerhulstWeights()
end program Main

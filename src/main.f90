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
!   17-Jan-2020 - Major improvements to the command-line interface.
!------------------------------------------------------------------------------


program Main
  use ModelParam
  use Penna, only: run, deallocAllocatables
  use RNG, only: assignRNGParams
  use CmdOptions, only: initializeCmdOptions, showHelpMsgAndNotes
  implicit none

  ! Initialize the command-line options.
  call initializeCmdOptions()

  ! Initialize the model parameters.
  call assignConfigFilePaths()
  call assignModelParams()

  ! Initialize the RNG with the provided command-line arguments.
  call assignRNGParams()

  ! Print the help message and stop the program if '-h' or '--help' is passed.
  call showHelpMsgAndNotes()

  ! Print the welcome text.
  call prettyPrintModelParams()

  ! Run the Penna model simulation.
  call run(MODEL_TIME_STEPS, MODEL_N0, MODEL_SAMPLE_SIZE, MODEL_REC_FLAG, &
      RECORD_TIME, PRINT_STATE /= SILENT_PRINT)

  ! Wrap up.
  call deallocAllocatables()
end program Main

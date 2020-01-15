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
  ! use Penna
  ! use CmdInterface
  ! use RNG, only: setSeed, chooseRNG
  use CmdOptions
  implicit none

  ! --------
  ! DEBUG
  ! --------
  call initializeCmdOptions()

  call parseCommandArguments()
  ! --------

  ! ! A unified record for the command-line arguments.
  ! type(CmdArgRecord) :: cmdArgs

  ! ! Get and then set model parameters
  ! ! ---------------------------------
  ! ! Initialize model parameters.
  ! call readModelParam()
  ! ! Get command-line arguments.
  ! call getCmdArgs(cmdArgs)

  ! ! Pretty print cmd arguments and model parameters.
  ! call printArgs(cmdArgs)
  ! ! ---------------------------------

  ! ! Initialize the random number generator.
  ! ! ---------------------------------------
  ! ! Choose RNG.
  ! call chooseRNG(cmdArgs%rngChoice)
  ! ! Set the seed for the chosen RNG.
  ! call setSeed(cmdArgs%rngSeed)
  ! ! ---------------------------------------

  ! ! Run the Penna model `sampleSize` times.
  ! call multipleRun(cmdArgs%maxTimestep, cmdArgs%startPopSize, &
  !     cmdArgs%sampleSize, cmdArgs%recordFlag, cmdArgs%toRecordTime)

  ! ! Wrap up. Deallocate any global allocatable variables.
  ! call wrapUp()
  ! if (.not.cmdArgs%isSilent) print "(*(a))", separator
end program Main

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
! REVISION:
!   11-Nov-2019 - First complete version of the program.
!   23-Nov-2019 - Major change to population handling.
!   06-Dec-2019 - Enhanced the population handling (via OO approach).
!------------------------------------------------------------------------------


program Main
  use Penna
  use CmdInterface
  implicit none

  ! -------------------------------------------------------------------------- !
  ! Arguments to run the simulation.
  integer :: timeSteps
  integer :: sampleSize
  integer :: startPopSize
  integer :: recordFlag
  logical :: isVerbosePrint
  logical :: toRecordTime
  ! -------------------------------------------------------------------------- !

  ! Initialize model parameters.
  call readModelParam

  ! Get command line arguments.
  call getCmdArgs(timeSteps, sampleSize, startPopSize, recordFlag, &
      isVerbosePrint, toRecordTime)

  ! Pretty print cmd arguments and model parameters.
  call printArgs(timeSteps, sampleSize, startPopSize, recordFlag, &
      isVerbosePrint)

  ! Run the Penna model multiple times.
  call multipleRun(timeSteps, startPopSize, sampleSize, recordFlag, &
      toRecordTime)

  ! Wrap up. Deallocate any global allocatable variables.
  call wrapUp
  print "(*(a))", separator
end program Main

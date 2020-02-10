!----------------------------------------------------------------------------- !
! PROGRAM: Asexual Penna model
!----------------------------------------------------------------------------- !
! AUTHOR: John Benedick A. Estrada
!----------------------------------------------------------------------------- !
!
! DESCRIPTION: 
!>  Implementation of the asexual Penna model based on the
!!  description of S. Oliveira [1].
!!
!!  However, there are differences in the implementation used in this 
!!  program and Oliveira's:
!!
!!    1.) Verhulst factor can change with age of the individuals.
!!        This can be used to model "survivability" by allowing
!!        the Verhulst factor to change such that individuals
!!        would have a lesser chance of dying randomly as they
!!        grow older.
!!    2.) Number of mutations per individual in the starting
!!        population can be selected.
!!
!!
!!  Reference:
!!  [1]  S. Oliveira. "Evolution, ageing and speciation: Monte Carlo
!!       simulations of biological systems", In: Brazilian Journal of
!!       Physics 34.3B (2004), pp. 1066-1076
!----------------------------------------------------------------------------- !
program Main
  use Penna
  implicit none

  ! Initialize the command-line options.
  call initializeProgram()

  ! Print the program parameters or the help text.
  call printInitialProgDetails()

  ! Simulate the Penna model.
  call run()

  ! Wrap up.
  call deallocAllocatables()
end program Main

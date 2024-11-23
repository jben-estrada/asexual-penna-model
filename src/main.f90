!----------------------------------------------------------------------------- !
! PROGRAM: Asexual Penna model
!----------------------------------------------------------------------------- !
! AUTHOR: John Benedick A. Estrada
!----------------------------------------------------------------------------- !
!
! DESCRIPTION: 
!>  Implementation of the asexual Penna model based on the original description
!!  by T.J.P. Penna [1].
!!
!!  However, there are differences in the implementation used in this 
!!  program:
!!
!!    1.) Age-dependent Verhulst factor.
!!    2.) Genome initialization of the population:
!!        2.a) The number of mutations per genome can be chosen.
!!        2.b) This number per genome can also be completely random.
!!
!!
!!  Reference:
!!  [1]  Thadeu Penna. "A Bit-String Model for Biological Aging".
!!       In: Journal of Statistical Physics 78 (Mar. 1995).
!!       DOI: 10.1007/BF02180147.
!----------------------------------------------------------------------------- !
program Main
  use Penna, only: initProgram, printProgDetails, run, freeAlloctbls
  implicit none

  ! Initialize the whole program.
  call initProgram()

  ! Print the program parameters or the help text.
  call printProgDetails()

  ! Simulate the Penna model.
  call run()

  ! Wrap up.
  call freeAlloctbls()
end program Main

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
!------------------------------------------------------------------------------


program Main
  use StdKind, only: timingRealKind
  use Penna, only: readModelParam, multipleRun, wrapUp
  implicit none

  ! -------------------------------------------------------------------------- !
  ! Arguments to run the simulation.
  integer :: timeSteps
  integer :: sampleSize_ 
  integer :: startPopSize_ 
  integer :: popArrSize
  integer :: recordFlag_
  real(kind=timingRealKind) :: meanTime
  ! Separator character array for pretty printing.
  integer              :: k  ! Index variable for `separator`
  character, parameter :: separator(29) = [("=", k = 1, 29)]
  ! -------------------------------------------------------------------------- !

  ! Initialize model parameters.
  call readModelParam

  ! Get command line arguments and pop array size.
  call getCmdArgs(timeSteps, sampleSize_, startPopSize_, recordFlag_)
  popArrSize = getPopArrSize(startPopSize_)

  ! Pretty print cmd arguments.
  call printArgs(timeSteps, sampleSize_, startPopSize_, recordFlag_)

  ! Run the Penna model multiple times.
  call multipleRun(timeSteps, startPopSize_, sampleSize_, popArrSize, &
      recordFlag_, meanTime)

  ! Wrap up. Deallocate allocatable arrays.
  call wrapUp
  print "(*(a))", separator
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getCmdArgs
  !>  Get command line arguments.
  ! -------------------------------------------------------------------------- !
  subroutine getCmdArgs(maxTimestep, sampleSize, startPopSize, recordFlag)
    use Model, only: MODEL_TIME_STEPS, MODEL_N0
    use SaveFormat, only: nullFlag
    implicit none

    integer, intent(out) :: maxTimestep
    integer, intent(out) :: sampleSize
    integer, intent(out) :: startPopSize
    integer, intent(out) :: recordFlag
    
    character(len=32) :: cmdArg
    integer           :: cmdInt
    integer           :: cmdError
    integer           :: i

    ! Default values for cmd arguments.
    maxTimestep = MODEL_TIME_STEPS
    sampleSize = 1
    startPopSize = MODEL_N0
    recordFlag = nullFlag

    do i = 1, 4
      call get_command_argument(i, cmdArg, status=cmdError)
      if (cmdError /= 0) cycle

      read(cmdArg, *) cmdInt
      select case (i)
      case (1)
        maxTimestep = cmdInt
      case (2)
        sampleSize = cmdInt
      case (3)
        startPopSize = cmdInt
      case (4)
        recordFlag = cmdInt
      end select
    end do
  end subroutine getCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printArgs
  !>  Print various parameters.
  ! -------------------------------------------------------------------------- !
  subroutine printArgs(maxTimestep, sampleSize, startPopSize, recordFlag)
    use SaveFormat, only: nullFlag
    implicit none

    integer, intent(in) :: maxTimestep
    integer, intent(in) :: sampleSize
    integer, intent(in) :: startPopSize
    integer, intent(in) :: recordFlag

    logical :: toRecord

    if (recordFlag /= nullFlag) then
      toRecord = .true.
    else
      toRecord = .false.
    end if

    print "(*(a))", separator 
    print "(a)", "Asexual Penna model"
    print "(*(a))", separator 
    print "(2(a20, i9/), a20, i9)", &
        "Number of time steps", maxTimestep, &
        "Sample size", sampleSize, &
        "Starting pop size", startPopSize
    print "(a20, L9)", "Record result", toRecord
    print "(*(a))", separator
  end subroutine printArgs


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getPopArrSize
  !>  Get the size of the population arrays.
  ! TODO: 
  !  Make a procedure that predicts array size base on the model params
  ! -------------------------------------------------------------------------- !
  function getPopArrSize(startPopSize) result(arrSize)
    use Model, only: MODEL_L, MODEL_R
    implicit none

    integer, intent(in) :: startPopSize
    integer             :: arrSize
    arrSize = startPopSize*(MODEL_L/MODEL_R + 1) + 1  ! Added an extra space
  end function getPopArrSize
end program Main

module Demographics
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Demographics
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing variables and procedures for recording demographics
  ! -------------------------------------------------------------------------- !
  implicit none
  private

  integer, public, allocatable, save :: demog_ageDstrb(:) 

  ! Time step range for recording demographics.
  integer, public, parameter :: DEF_DEMOG_LAST_STEP = 300
  integer, public, save      :: DEMOG_LAST_STEPS = DEF_DEMOG_LAST_STEP

  public :: resetDstrbs
  public :: updateAgeDstrb
  public :: deallocDstrb
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: resetDstrbs
  !>  Reset the demographics.
  ! -------------------------------------------------------------------------- !
  subroutine resetDstrbs
    use ModelParam, only: MODEL_L
    implicit none

    if (.not.allocated(demog_ageDstrb)) allocate(demog_ageDstrb(0:MODEL_L))

    demog_ageDstrb(:) = 0
  end subroutine resetDstrbs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateAgeDstrb
  !>  Update the age demographics.
  ! -------------------------------------------------------------------------- !
  subroutine updateAgeDstrb(age, dstrb)
    implicit none
  
    integer, intent(in)    :: age
    integer, intent(inout) :: dstrb(:)

    dstrb(age) = dstrb(age) + 1
  end subroutine updateAgeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocDstrb
  !>  Deallocate demographic arrays
  ! -------------------------------------------------------------------------- !
  subroutine deallocDstrb
    implicit none

    if (allocated(demog_ageDstrb)) deallocate(demog_ageDstrb)
  end subroutine deallocDstrb
end module Demographics

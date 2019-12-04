module Demographics
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Demographics
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing variables and procedures for recording demographics
  ! -------------------------------------------------------------------------- !
  use Gene ! `personIK` of StdKind comes from here as well.
  implicit none
  private

  integer(kind=personIK), public, allocatable, save :: demog_genomeDstrb(:)
  integer,                public, allocatable, save :: demog_ageDstrb(:) 
  
  integer, public :: DEMOG_LAST_STEPS = 300

  public :: resetDstrbs
  public :: updateAgeDstrb
  public :: updateGenomeDstrb
  public :: deallocDstrb
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: reset_dstrbs
  !>  Reset the demographics.
  ! -------------------------------------------------------------------------- !
  subroutine resetDstrbs
    use ModelParam, only: MODEL_L
    implicit none

    if (.not.allocated(demog_ageDstrb)) then
      allocate(demog_ageDstrb(0:MODEL_L))
    end if

    if (.not.allocated(demog_genomeDstrb)) then
      allocate(demog_genomeDstrb(MODEL_L))
    end if

    demog_ageDstrb(:) = 0
    demog_genomeDstrb(:) = 0_personIK
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
  ! SUBROUTINE: updateGenomeDstrb
  !>  Update the genome demographics.
  ! -------------------------------------------------------------------------- !
  subroutine updateGenomeDstrb(genome, genomeDstrb)
    use ModelParam, only: MODEL_L
    implicit none

    integer(kind=personIK), intent(inout) :: genomeDstrb(:)
    integer(kind=personIK), intent(in)    :: genome
    integer(kind=personIK)                :: i

    do i = 1, MODEL_L
      if (getBinDigit(genome, i) == GENE_UNHEALTHY) then
        genomeDstrb(i) = genomeDstrb(i) + 1
      end if
    end do
  end subroutine updateGenomeDstrb


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getBinDigit
  !>  Get the `k`th binary digit of the integer `number`.
  ! -------------------------------------------------------------------------- !
  function getBinDigit(number, k) result(bit)
    implicit none
    integer(kind=personIK), intent(in) :: number
    integer(kind=personIK), intent(in) :: k
    integer(kind=personIK)             :: bit

    bit = iand(shiftr(number, k - 1), 1_personIK)
  end function getBinDigit


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocDstrb
  !>  Deallocate demographic arrays
  ! -------------------------------------------------------------------------- !
  subroutine deallocDstrb
    implicit none
    if (allocated(demog_ageDstrb)) deallocate(demog_ageDstrb)
    if (allocated(demog_genomeDstrb)) deallocate(demog_genomeDstrb)
  end subroutine deallocDstrb
end module Demographics

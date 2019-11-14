module Demographics
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Demographics
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing variables and procedures for recording demographics
  ! -------------------------------------------------------------------------- !
  use Gene
  use StdKind, only: personRealKind, personIntKind
  implicit none
  private

  real(kind=personRealKind), public, allocatable, save :: demog_ageDstrb(:) 
  real(kind=personRealKind), public, allocatable, save :: demog_genomeDstrb(:)
  
  integer, public, parameter :: DEMOG_LAST_STEPS = 300

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
    use Model, only: MODEL_L
    implicit none

    if (.not.allocated(demog_ageDstrb)) then
      allocate(demog_ageDstrb(0:MODEL_L))
    end if

    if (.not.allocated(demog_genomeDstrb)) then
      allocate(demog_genomeDstrb(MODEL_L))
    end if

    demog_ageDstrb(:) = 0._personRealKind
    demog_genomeDstrb(:) = 0._personRealKind
  end subroutine resetDstrbs

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateAgeDstrb
  !>  Update the age demographics.
  ! -------------------------------------------------------------------------- !
  subroutine updateAgeDstrb(age, dstrb)
    implicit none
  
    integer(kind=personIntKind), intent(in)    :: age
    real(kind=personRealKind),   intent(inout) :: dstrb(:)

    dstrb(age) = dstrb(age) + 1
  end subroutine updateAgeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateGenomeDstrb
  !>  Update the genome demographics.
  ! -------------------------------------------------------------------------- !
  subroutine updateGenomeDstrb(genome, genomeDstrb)
    use Model, only: MODEL_L
    implicit none

    real(kind=personIntKind),    intent(inout) :: genomeDstrb(:)
    integer(kind=personIntKind), intent(in)    :: genome
    integer(kind=personIntKind)                :: i

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
    integer(kind=personIntKind), intent(in) :: number
    integer(kind=personIntKind), intent(in) :: k
    integer(kind=personIntKind)             :: bit

    bit = iand(shiftr(number, k - 1), 1_personIntKind)
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

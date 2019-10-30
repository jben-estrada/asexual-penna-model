module Demographics
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Demographics
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing variables and procedures for recording demographics
  ! -------------------------------------------------------------------------- !
  use Model
  use Gene
  implicit none
  integer, parameter :: DEMOG_LAST_STEPS = 300

  real(kind=stdRealKind), allocatable, save :: demog_ageDstrb(:) 
  real(kind=stdRealKind), allocatable, save :: demog_genomeDstrb(:)

  interface writeDemog
    procedure :: writeDemogReal, writeDemogInt
  end interface writeDemog
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: reset_dstrbs
  !>  Reset the demographics.
  ! -------------------------------------------------------------------------- !
  subroutine resetDstrbs
    implicit none
    if (.not.allocated(demog_ageDstrb)) then
      allocate(demog_ageDstrb(0:MODEL_L))
    end if
    if (.not.allocated(demog_genomeDstrb)) then
      allocate(demog_genomeDstrb(MODEL_L))
    end if
    demog_ageDstrb(:) = 0._stdRealKind
    demog_genomeDstrb(:) = 0._stdRealKind
  end subroutine resetDstrbs

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateAgeDstrb
  !>  Update the age demographics.
  ! -------------------------------------------------------------------------- !
  subroutine updateAgeDstrb(age, dstrb)
    implicit none
    integer(kind=stdIntKind), intent(in) :: age
    real(kind=stdRealKind), intent(inout) :: dstrb(:)

    dstrb(age) = dstrb(age) + 1
  end subroutine updateAgeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateGenomeDstrb
  !>  Update the genome demographics.
  !   TODO
  ! -------------------------------------------------------------------------- !
  subroutine updateGenomeDstrb(genome, genomeDstrb)
    implicit none
    real(kind=stdIntKind), intent(inout) :: genomeDstrb(:)
    integer(kind=stdIntKind), intent(in) :: genome
    integer(kind=stdIntKind) :: i

    do i = 1, MODEL_L
      if (getBinDigit(genome, i) == GENE_UNHEALTHY) then
        genomeDstrb(i) = genomeDstrb(i) + 1
      end if
    end do
  end subroutine updateGenomeDstrb


  function getBinDigit(number, k) result(bit)
    implicit none
    integer(kind=stdIntKind), intent(in) :: number
    integer(kind=stdIntKind), intent(in) :: k
    integer(kind=stdIntKind) :: bit

    bit = iand(shiftr(number, k - 1), 1)
  end function getBinDigit


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writeDemogReal
  !>  Write `dstrb` array of type real(8) to an external file.
  !   NOTE: DEPRECATED
  ! -------------------------------------------------------------------------- !
  subroutine writeDemogReal(dstrb, unit, format)
    implicit none
    real(kind=real64), intent(in) :: dstrb
    character(len=*), intent(in) :: format
    integer, intent(in) :: unit

    write(unit, format) dstrb
  end subroutine writeDemogReal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writeDemogInt
  !>  Write `dstrb` array of type integer(4) to an external file.
  !   NOTE: DEPRECATED
  ! -------------------------------------------------------------------------- !
  subroutine writeDemogInt(dstrb, unit, format)
    implicit none
    integer, intent(in) :: dstrb
    integer, intent(in) :: unit
    character(len=*), intent(in) :: format

    write(unit, format) dstrb
  end subroutine writeDemogInt

  
  subroutine deallocDstrb
    implicit none
    if (allocated(demog_ageDstrb)) deallocate(demog_ageDstrb)
    if (allocated(demog_genomeDstrb)) deallocate(demog_genomeDstrb)
  end subroutine deallocDstrb

end module Demographics
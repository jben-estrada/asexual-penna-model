module Demographics
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Demographics
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing variables and procedures for recording demographics
  ! -------------------------------------------------------------------------- !
  use Gene, only: personIK
  implicit none
  private
  
  ! AGE DISTRIBUTION
  ! -------------------------------------------------------------------------- !
  ! Age distribution.
  integer, public, allocatable, save :: ageDistribution(:) 
  
  ! Time step range for recording demographics.
  integer, public, parameter :: DEF_DEMOG_LAST_STEP = 300
  integer, public, save      :: DEMOG_LAST_STEPS = DEF_DEMOG_LAST_STEP

  public :: resetAgeDstrb
  public :: updateAgeDstrb
  public :: deallocAgeDstrb

  ! SHANNON DIVERSITY INDEX AND GENOME DISTRIBUTION.
  ! -------------------------------------------------------------------------- !
  ! Genome count. This should be equal to population size.
  integer, save :: genomeCount = 0

  ! Node type for genome distribution lists.
  type GenomeDstrbNode
    integer(kind=personIK)         :: genome
    integer                        :: count = 0
    type(GenomeDstrbNode), pointer :: next => null()
  end type
  
  ! Head of genome distribution list.
  type(GenomeDstrbNode), pointer :: genomeDstrbHead => null()
  type(GenomeDstrbNode), pointer :: genomeDstrbTail => null()
  
  ! TODO: Test Shannon diversity index!

  public :: updateGenomeDstrb
  public :: freeGenomeDstrbList
  public :: getDiversityIdx
  ! -------------------------------------------------------------------------- !
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateGenomeDstrb
  !>  Update the genome distribution list. It searches for the matching
  !!  genome and increment the count of the matching genome if found.
  !!  If no genome is found, a new node in the list is created containing
  !!  the non-matching genome.
  ! -------------------------------------------------------------------------- !
  subroutine updateGenomeDstrb(genome)
    implicit none
    integer(kind=personIK), intent(in) :: genome

    call incrementGenomeCount(genomeDstrbHead, genome)
    genomeCount = genomeCount + 1
  end subroutine updateGenomeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: incrementGenomeCount
  !>  Increment genome count and update 
  ! -------------------------------------------------------------------------- !
  recursive subroutine incrementGenomeCount(node, genome)
    implicit none

    type(GenomeDstrbNode), pointer, intent(inout) :: node
    integer(kind=personIK),         intent(in)    :: genome

    if (associated(node)) then
      ! Update genome distribution if match is found.
      if (node%genome == genome) then
        node%count = node%count + 1
      else
        call incrementGenomeCount(node%next, genome)
      end if

    else
      ! Create a new node if no match is found.
      call appendGenomeDstrbNode(genome)
    end if
  end subroutine incrementGenomeCount


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: appendGenomeDstrbNode
  !>  Allocate new node and append it to the end of the genome distribution
  !!  list.
  ! -------------------------------------------------------------------------- !
  subroutine appendGenomeDstrbNode(genome)
    implicit none
    integer(kind=personIK), intent(in) :: genome

    type(GenomeDstrbNode), pointer :: new

    ! Allocate new node.
    new => null()
    allocate(new)

    ! Initialize the new node.
    new % genome = genome
    new % count  = 1
    new % next   => null()
    
    ! Assign head if the list is not yet initialized. 
    if (.not.associated(genomeDstrbHead)) &
        genomeDstrbHead => new
    ! Append the new node at the end of the list.
    if (associated(genomeDstrbTail)) &
        genomeDstrbTail%next => new
    genomeDstrbTail => new
  end subroutine appendGenomeDstrbNode


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeGenomeDstrbList
  !>  Free allocated nodes of the genome distribution list and reset counters.
  ! -------------------------------------------------------------------------- !
  subroutine freeGenomeDstrbList()
    implicit none
  
    genomeDstrbTail => null()
    genomeCount = 0
    call cascadeFreeNodes(genomeDstrbHead)
  end subroutine freeGenomeDstrbList


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getDiversityIdx
  !>  Calculate the Shannon diversity index of the current genome distribution
  !!  list.
  ! -------------------------------------------------------------------------- !
  function getDiversityIdx() result(diversityIdx)
    implicit none
    real :: diversityIdx

    type(GenomeDstrbNode), pointer :: reader
    reader => genomeDstrbHead

    ! Reset the Shannon diversity index just to be sure.
    diversityIdx = 0

    ! Read genome count of each node in the genome distribution list.
    do
      if (associated(reader)) then
        diversityIdx = diversityIdx - real(reader%count)/real(genomeCount) * &
            log(real(reader%count)/real(genomeCount))

        ! Proceed to the next element of the genome distribution list.
        reader => reader%next
      else
        exit
      end if
    end do
  end function getDiversityIdx


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: cascadeFreeNodes
  !>  Free allocated nodes.
  ! -------------------------------------------------------------------------- !
  recursive subroutine cascadeFreeNodes(node)
    implicit none
    type(GenomeDstrbNode), pointer, intent(inout) :: node

    if (associated(node)) then
      call cascadeFreeNodes(node%next)
      deallocate(node)
    end if
  end subroutine cascadeFreeNodes


  ! -------------------------------------------------------------------------- !
  ! AGE DISTRIBUTION
  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: resetAgeDstrb
  !>  Reset the demographics.
  ! -------------------------------------------------------------------------- !
  subroutine resetAgeDstrb
    use ModelParam, only: MODEL_L
    implicit none

    if (.not.allocated(ageDistribution)) allocate(ageDistribution(0:MODEL_L))
    ageDistribution(:) = 0
  end subroutine resetAgeDstrb


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
  subroutine deallocAgeDstrb
    implicit none

    if (allocated(ageDistribution)) deallocate(ageDistribution)
  end subroutine deallocAgeDstrb
end module Demographics

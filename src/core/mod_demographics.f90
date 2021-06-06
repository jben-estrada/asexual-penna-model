module Demographics
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Demographics
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing variables and procedures for recording demographics
  ! -------------------------------------------------------------------------- !
  use Parameters, only: MODEL_L
  use Gene, only: GENE_UNHEALTHY
  use ErrorMSG, only: raiseError
  use DynamicBitSet, only: BitSet, operator(==)
  implicit none
  private
  
  ! AGE DISTRIBUTION
  ! -------------------------------------------------------------------------- !
  integer, allocatable :: ageDistribution(:) 
    !! Age distribution.
  
  ! Time step range for recording demographics.
  integer, parameter :: DEF_DEMOG_LAST_STEP = 300
    !! Default final time steps when to record the age distribution.
  integer            :: DEMOG_LAST_STEPS = DEF_DEMOG_LAST_STEP
    !! Final time steps when when to record the age distribution.
    !! Defaults to `DEF_DEMOG_LAST_STEP`. Changes to a negative value if
    !! the age distribution is not to be recorded.

  public :: ageDistribution
  public :: DEF_DEMOG_LAST_STEP
  public :: DEMOG_LAST_STEPS

  public :: resetAgeDstrb
  public :: updateAgeDstrb
  public :: deallocAgeDstrb

  ! SHANNON DIVERSITY INDEX, GENOME DISTRIBUTION & BAD GENE DISTRIBUTION.
  ! -------------------------------------------------------------------------- !
  ! Genome count. This should be equal to population size.
  integer :: genomeCount = 0

  type GenomeDstrbNode
    !! Node type for genome distribution lists.
    type(BitSet) :: genome
      !! The genome of this `GenomeDstrbNode` object.
    integer      :: count = 0
      !! Count of `Person` objects with the same value for `genome`
      !! as in this `GenomeDstrbNode` object.

    type(GenomeDstrbNode), pointer :: next => null()
      !! Pointer to the element of genome distribution list.
  end type

  type(GenomeDstrbNode), pointer :: genomeDstrbHead => null()
    !! Head of the genome distribution list.
  type(GenomeDstrbNode), pointer :: genomeDstrbTail => null()
    !! Tail of the genome distribution list.

  public :: updateGenomeDstrb
  public :: freeGenomeDstrbList
  public :: getDiversityIdx
  public :: getBadGeneDstrb
  public :: getUniqueGenomeCount
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateGenomeDstrb
  !>  Update the genome distribution list. It searches for the matching
  !!  genome and increment the count of the matching genome if found.
  !!  If no genome is found, a new node in the list is created containing
  !!  the non-matching genome.
  ! -------------------------------------------------------------------------- !
  subroutine updateGenomeDstrb(genome)
    type(BitSet), intent(in) :: genome
      !! The genome to be added to the genome distribution.

    call incrementGenomeCount(genome)
    genomeCount = genomeCount + 1
  end subroutine updateGenomeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: incrementGenomeCount
  !>  Increment genome count and update.
  ! -------------------------------------------------------------------------- !
  subroutine incrementGenomeCount(genome)
    type(BitSet), intent(in) :: genome
    type(GenomeDstrbNode), pointer :: currentNode

    currentNode => genomeDstrbHead
    genomeDstrb: do
      if (associated(currentNode)) then
        if (currentNode%genome == genome) then
          currentNode%count = currentNode%count + 1
          exit genomeDstrb
        else
          currentNode => currentNode%next
        end if
      else
        ! Create a new node if no match is found.
        call appendGenomeDstrbNode(genome)
        exit genomeDstrb
      end if
    end do genomeDstrb
  end subroutine incrementGenomeCount


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: appendGenomeDstrbNode
  !>  Allocate new node and append it to the end of the genome distribution
  !!  list.
  ! -------------------------------------------------------------------------- !
  subroutine appendGenomeDstrbNode(genome)
    type(BitSet), intent(in) :: genome
      !! The genome the `GenomeDstrbNode` will contain.

    type(GenomeDstrbNode), pointer :: new

    ! Allocate new node.
    new => null()
    allocate(new)

    ! Initialize the new node.
    new % genome = genome
    new % count  = 1
    new % next   => null()

    if (associated(genomeDstrbHead) .and. associated(genomeDstrbTail)) then
      genomeDstrbTail%next => new
    else if (.not.associated(genomeDstrbHead) .and. &
             .not.associated(genomeDstrbTail)) then
      ! Assign head if the list is not yet initialized. 
      genomeDstrbHead => new
    else
      call raiseError("Internal error. Invalid genome distribution list.")
    end if

    genomeDstrbTail => new
  end subroutine appendGenomeDstrbNode


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeGenomeDstrbList
  !>  Free allocated nodes of the genome distribution list and reset counters.
  ! -------------------------------------------------------------------------- !
  subroutine freeGenomeDstrbList()
    type(GenomeDstrbNode), pointer :: currentNode
    type(GenomeDstrbNode), pointer :: deletedNode

    currentNode => genomeDstrbHead
    deletedNode => null()
    do
      if (associated(currentNode)) then
        deletedNode => currentNode
        currentNode => currentNode%next

        deallocate(deletedNode)
      else
        exit
      end if
    end do
  
    genomeDstrbHead => null()
    genomeDstrbTail => null()
    genomeCount = 0
  end subroutine freeGenomeDstrbList


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getDiversityIdx
  !>  Calculate the Shannon diversity index of the current genome distribution
  !!  list.
  ! -------------------------------------------------------------------------- !
  function getDiversityIdx() result(diversityIdx)
    real :: diversityIdx

    type(GenomeDstrbNode), pointer :: reader
    reader => genomeDstrbHead

    ! Reset the Shannon diversity index just to be sure.
    diversityIdx = 0

    ! Read genome count of each node in the genome distribution list.
    do
      if (associated(reader)) then
        diversityIdx = diversityIdx - real(reader % count)/real(genomeCount) * &
            log(real(reader % count)/real(genomeCount))

        ! Proceed to the next element of the genome distribution list.
        reader => reader % next
      else
        exit
      end if
    end do
  end function getDiversityIdx


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getBadGeneDstrb
  !>  Get the distribution of bad genes in the population's genomes.
  ! -------------------------------------------------------------------------- !
  function getBadGeneDstrb() result(badGeneDstrb)
    integer :: badGeneDstrb(MODEL_L)

    type(GenomeDstrbNode), pointer :: reader
    integer :: i

    badGeneDstrb(:) = 0
    reader => genomeDstrbHead

    do
      if (associated(reader)) then

        ! Count the bad genes of the current genome.
        do i = 1, MODEL_L
          if (reader%genome%get(i) .eqv. GENE_UNHEALTHY) &
            badGeneDstrb(i) = badGeneDstrb(i) + reader%count
        end do

        ! Get to the next element of genome distribution list.
        reader => reader % next
      else
        exit
      end if
    end do
  end function getBadGeneDstrb


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getUniqueGenomeCount
  !>  Get the number of unique genomes.
  ! -------------------------------------------------------------------------- !
  function getUniqueGenomeCount() result(uniqueGeneCount)
    type(GenomeDstrbNode), pointer :: reader
    integer :: uniqueGeneCount
    
    uniqueGeneCount = 0

    reader => genomeDstrbHead
    do
      if (associated(reader)) then
        uniqueGeneCount = uniqueGeneCount + 1

        ! Get to the next element of genome distribution list.
        reader => reader % next
      else
        exit
      end if
    end do
  end function getUniqueGenomeCount



  ! -------------------------------------------------------------------------- !
  ! AGE DISTRIBUTION
  !

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: resetAgeDstrb
  !>  Reset the demographics.
  ! -------------------------------------------------------------------------- !
  subroutine resetAgeDstrb(genomeLen)
    integer, intent(in) :: genomeLen
      !! Genome length which also corresponds to age demographics range.

    if (.not.allocated(ageDistribution)) allocate(ageDistribution(0:genomeLen))
    ageDistribution(:) = 0
  end subroutine resetAgeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateAgeDstrb
  !>  Update the age demographics.
  ! -------------------------------------------------------------------------- !
  subroutine updateAgeDstrb(age)
    integer, intent(in) :: age
      !! The age to be added into the age distribution.

    ageDistribution(age) = ageDistribution(age) + 1
  end subroutine updateAgeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocDstrb
  !>  Deallocate demographic arrays
  ! -------------------------------------------------------------------------- !
  subroutine deallocAgeDstrb()
    if (allocated(ageDistribution)) deallocate(ageDistribution)
  end subroutine deallocAgeDstrb
end module Demographics

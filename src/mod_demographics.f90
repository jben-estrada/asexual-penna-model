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
  use DynamicBitSetType, only: BitSet, operator(==)
  use CastProcs, only: isFinite
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

  ! DIVERSITY INDEX, GENOME DISTRIBUTION & BAD GENE DISTRIBUTION.
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

  public :: addGenomeToDstrb
  public :: delGenomeFromDstrb
  public :: freeGenomeDstrbList
  public :: getDiversityIdx
  public :: getBadGeneDstrb
  public :: getUniqueGenomeCount
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: addGenomeToDstrb
  !>  Add a genome to the genome distribution
  ! -------------------------------------------------------------------------- !
  subroutine addGenomeToDstrb(genome)
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

    genomeCount = genomeCount + 1
  end subroutine addGenomeToDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: delGenomeFromDstrb
  !>  Delete a genome from the distribution
  ! -------------------------------------------------------------------------- !
  subroutine delGenomeFromDstrb(genome)
    type(BitSet), intent(in) :: genome
    type(GenomeDstrbNode), pointer :: currentNode
    type(GenomeDstrbNode), pointer :: prevNode
    type(GenomeDstrbNode), pointer :: nextNode

    prevNode => null()
    currentNode => genomeDstrbHead

    genomeDstrb: do
      if (associated(currentNode)) then
        if (currentNode%genome == genome) then
          currentNode%count = currentNode%count - 1

          ! Delete the current node
          if (currentNode%count <= 0) then
            nextNode => currentNode%next
            if (associated(prevNode)) prevNode%next => nextNode
            if (associated(currentNode, genomeDstrbTail)) &
                genomeDstrbTail => prevNode
            if (associated(currentNode, genomeDstrbHead)) &
                genomeDstrbHead => genomeDstrbHead%next
            deallocate(currentNode)
          end if

          exit genomeDstrb
        else
          prevNode => currentNode
          currentNode => currentNode%next
        end if
      else
        call raiseError("Cannot find genome to remove from dstrb.")
      end if
    end do genomeDstrb

    genomeCount = genomeCount - 1
  end subroutine delGenomeFromDstrb


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
  ! SUBROUTINE: getShannonEntropy
  !>  Calculate the unnormalized Shannon entropy
  ! -------------------------------------------------------------------------- !
  function getShannonEntropy() result(entropy)
    real :: entropy

    type(GenomeDstrbNode), pointer :: reader
    reader => genomeDstrbHead

    ! Reset the Shannon diversity index just to be sure.
    entropy = 0
    ! Read genome count of each node in the genome distribution list.
    do
      if (associated(reader)) then
        entropy = entropy - real(reader % count)/real(genomeCount) * &
            log(real(reader % count)/real(genomeCount))
        ! Proceed to the next element of the genome distribution list.
        reader => reader % next
      else
        exit
      end if
    end do
  end function getShannonEntropy


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getNormalizedShannon
  !>  Calculate the normalized Shannon entropy
  ! -------------------------------------------------------------------------- !
  function getNormalizedShannon() result(entropy)
    real :: entropy

    type(GenomeDstrbNode), pointer :: reader
    reader => genomeDstrbHead

    ! Reset the Shannon diversity index just to be sure.
    entropy = 0.0
    ! Read genome count of each node in the genome distribution list.
    do
      if (associated(reader)) then
        entropy = entropy - real(reader % count)/real(genomeCount) * &
            log(real(reader % count)/real(genomeCount))

        ! Proceed to the next element of the genome distribution list.
        reader => reader % next
      else
        exit
      end if
    end do

    ! Normalize the diversity index
    entropy = entropy / log(real(genomeCount))
  end function getNormalizedShannon


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getRenyiEntropy
  !>  Calculate the Renyi Entropy of order alpha
  !!  a generalization of different entropies including Shannon entropy.
  ! -------------------------------------------------------------------------- !
  function getRenyiEntropy(alpha) result(entropy)
    real, intent(in) :: alpha
    real :: entropy
    real :: probabilitySum

    real, parameter :: zeroCmpEpsilon = 1e-6

    type(GenomeDstrbNode), pointer :: reader
    reader => genomeDstrbHead
  
    ! Initialize output
    entropy = 0.0
    probabilitySum = 0.0

    ! If alpha is equal to 1.0, use the definition of Shannon entropy
    if (abs(alpha - 1.0) < zeroCmpEpsilon) then
      entropy = getShannonEntropy()
      return
    end if

    do
      if (associated(reader)) then
        probabilitySum = probabilitySum + &
            (real(reader % count)/real(genomeCount)) ** alpha
        ! Proceed to the next element of the genome distribution list.
        reader => reader % next
      else
        exit
      end if
    end do

    entropy = log(probabilitySum) / (1 - alpha)
  end function getRenyiEntropy


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getDiversityIdx
  !>  Calculate the genetic diversity index of the current genome distribution
  !!  list. The input parameter `alpha` determines the kind of entropy to be
  !!  used. Positive real values for alpha corresponds to Renyi entropy of
  !!  order alpha. Negative real values for alpha corresponds to the
  !!  normalized Shannon entropy which is also the default behavior.
  ! -------------------------------------------------------------------------- !
  function getDiversityIdx(alpha) result(diversityIdx)
    real, intent(in), optional :: alpha
    real :: diversityIdx
    
    ! If the population goes extinct, we set the diversity index to 0
    if (genomeCount == 0) then
      diversityIdx = 0.0
      return
    end if

    if (present(alpha)) then
      if (isFinite(alpha)) then
        diversityIdx = getRenyiEntropy(alpha)
        return
      end if
    end if

    diversityIdx = getNormalizedShannon()
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

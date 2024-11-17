module Demographics
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Demographics
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing variables and procedures for recording demographics
  ! -------------------------------------------------------------------------- !
  use Parameters, only: MODEL_L, MODEL_AGE_DSTRB_INIT_TIMESTEP
  use Gene, only: GENE_UNHEALTHY
  use ErrorMSG, only: raiseError
  use CastProcs, only: isFinite, logicalToInt
  use StaticBitSetType, only: &
    StaticBitSet,      &
    maskBitset,        &
    hashBitSet,        &
    bitSetHashKind,    &
    extractBitSetData, &
    operator(==)
  implicit none
  private
  
  ! AGE DISTRIBUTION
  ! -------------------------------------------------------------------------- !
  integer, allocatable :: ageDistribution(:) 
    !! Age distribution.

  public :: ageDistribution
  public :: resetAgeDstrb
  public :: updateAgeDstrb
  public :: deallocAgeDstrb

  ! DIVERSITY INDEX, GENOME DISTRIBUTION & BAD GENE DISTRIBUTION.
  ! -------------------------------------------------------------------------- !
  type :: GenomeBin
    !! A bin for the genome distribution containing the genome and its 
    !! number of bearers.

    type(StaticBitSet) :: genome
      !! The genome of this `GenomeDstrbNode` object.
    integer            :: count
      !! Count of `Person` objects with the same value for `genome`
      !! as in this `GenomeDstrbNode` object.
  end type GenomeBin

  type(GenomeBin), allocatable :: genomeDstrb(:)
    !! A set of genomes in the population with their corresponding bearer count.
  integer :: genomeDstrbSize    = 0
  integer :: genomeDstrbMaxSize = 0

  integer, allocatable :: genomeIdxArray(:)
    !! Array of genome locations in the distribution for ease of iteration in
    !! calculating the Renyi entropies: e.g. Shannon entropy.
  integer :: uniqueGenomeCount = 0
    !! Number of active unique genomes in the population.
    !! Also the size of `genomeIdxArray`
  integer :: genomeIdxMaxSize = 0

  ! Genome count. This should be equal to population size.
  integer :: totalGenomeCount = 0
  
  integer, parameter :: INIT_GENOME_DSTRB_SIZE = 100
  real,    parameter :: MAX_LOAD_FACTOR = 0.75
  real,    parameter :: GROWTH_FACTOR = 1.5

  public :: initGenomeDstrb
  public :: freeGenomeDstrb
  public :: addGenomeToDstrb
  public :: delGenomeFromDstrb
  public :: getDiversityIdx
  public :: getBadGeneDstrb
  public :: getUniqueGenomeCount
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initGenomeDstrb
  !>  Initialize the hash map, genome distribution.
  ! -------------------------------------------------------------------------- !
  subroutine initGenomeDstrb()
    allocate(genomeDstrb(0: INIT_GENOME_DSTRB_SIZE - 1))
    genomeDstrbMaxSize = INIT_GENOME_DSTRB_SIZE
    genomeDstrbSize = 0

    allocate(genomeIdxArray(INIT_GENOME_DSTRB_SIZE))
    genomeIdxMaxSize = INIT_GENOME_DSTRB_SIZE
    uniqueGenomeCount = 0
  end subroutine initGenomeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeGenomeDstrb
  !>  Free the genome distribution and its content.
  ! -------------------------------------------------------------------------- !
  subroutine freeGenomeDstrb()
    if (allocated(genomeDstrb)) deallocate(genomeDstrb)
    genomeDstrbSize = 0
    genomeDstrbMaxSize = 0

    if (allocated(genomeIdxArray)) deallocate(genomeIdxArray)
    genomeIdxMaxSize  = 0
    uniqueGenomeCount = 0
    totalGenomeCount  = 0
  end subroutine freeGenomeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: addGenomeToDstrb
  !>  Add a genome to the genome distribution.
  ! -------------------------------------------------------------------------- !
  subroutine addGenomeToDstrb(genome, mask)
    type(StaticBitSet), intent(in) :: genome
    logical,            intent(in) :: mask(:)
    type(StaticBitSet) :: maskedGenome
    integer :: genomeIdx

    if (real(genomeDstrbSize)/real(genomeDstrbMaxSize) > MAX_LOAD_FACTOR) then
      call rehashGenomeDstrb(mask)
    end if

    call maskBitset(genome, mask, maskedGenome)
    genomeIdx = findGenomeIdx(maskedGenome)

    ! If the genome is not yet in the distribution, initialize it.
    if (.not.genomeDstrb(genomeIdx)%genome%isInitialized()) then      
      ! Initialize the new entry in the distribution
      genomeDstrb(genomeIdx)%genome = maskedGenome
      genomeDstrb(genomeIdx)%count  = 0
      genomeDstrbSize = genomeDstrbSize + 1

      ! Register the new genome for iterating later on.
      uniqueGenomeCount = uniqueGenomeCount + 1
      genomeIdxArray(uniqueGenomeCount) = genomeIdx
    end if

    genomeDstrb(genomeIdx)%count = genomeDstrb(genomeIdx)%count + 1
    totalGenomeCount = totalGenomeCount + 1
  end subroutine addGenomeToDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: delGenomeFromDstrb
  !>  Delete a distribution from the genome distribution.
  ! -------------------------------------------------------------------------- !
  subroutine delGenomeFromDstrb(genome, mask)
    type(StaticBitSet), intent(in) :: genome
    logical,            intent(in) :: mask(:)
    type(StaticBitSet) :: maskedGenome
    integer :: genomeIdx
    
    call maskBitset(genome, mask, maskedGenome)
    genomeIdx = findGenomeIdx(maskedGenome)

    if (genomeDstrb(genomeIdx)%count == 0) then
      call raiseError(  &
          "Cannot delete a genome from the genome distribution. " //  &
          "The genome count is already at 0."  &
        )
    end if

    genomeDstrb(genomeIdx)%count = genomeDstrb(genomeIdx)%count - 1
    totalGenomeCount = totalGenomeCount - 1
  end subroutine delGenomeFromDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: rehashGenomeDstrb
  !>  Rehash the genome distribution, a hash map, to increase its maximum size.
  ! -------------------------------------------------------------------------- !
  subroutine rehashGenomeDstrb(mask)
    logical, intent(in) :: mask(:)

    type(GenomeBin), allocatable :: tempGenomeDstrb(:)
    type(StaticBitSet) :: maskedGenome
    integer :: newSize, genomeIdx, i

    newSize = int(genomeDstrbMaxSize * GROWTH_FACTOR)

    if (allocated(tempGenomeDstrb)) deallocate(tempGenomeDstrb)
    call move_alloc(genomeDstrb, tempGenomeDstrb)
    allocate(genomeDstrb(0: newSize - 1))
    genomeDstrbMaxSize = newSize

    ! Also extend the genome index array and prepare to refresh its content.
    call extendGenomeIdxArray()
    uniqueGenomeCount = 0
    
    ! Reinsert all genomes
    do i = lbound(tempGenomeDstrb, 1), ubound(tempGenomeDstrb, 1)
      if ( .not. tempGenomeDstrb(i)%genome%isInitialized() ) cycle

      call maskBitset(tempGenomeDstrb(i)%genome, mask, maskedGenome)
      genomeIdx = findGenomeIdx(maskedGenome)

      genomeDstrb(genomeIdx)%genome = maskedGenome
      genomeDstrb(genomeIdx)%count = tempGenomeDstrb(i)%count

      uniqueGenomeCount = uniqueGenomeCount + 1
      genomeIdxArray(uniqueGenomeCount) = genomeIdx
    end do
  end subroutine rehashGenomeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: extendGenomeIdxArray
  !>  Extend the genome index array to accomodate more genomes.
  ! -------------------------------------------------------------------------- !
  subroutine extendGenomeIdxArray()
    integer :: newSize

    newSize = int(genomeIdxMaxSize * GROWTH_FACTOR)
    deallocate(genomeIdxArray)
    allocate(genomeIdxArray(newSize))

    genomeIdxMaxSize = newSize
  end subroutine extendGenomeIdxArray


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: findGenomeIdx
  !>  Find the corresponding index for the input genome. The genome
  !!  distribution hash map resolves collision by linear probing for simpler
  !!  implementation and to ease the finalization of the genome distribition.
  ! -------------------------------------------------------------------------- !
  integer function findGenomeIdx(genome) result(idx)
    type(StaticBitSet), intent(in)  :: genome

    idx = int(                                          &
        modulo(                                         &
          hashBitSet(genome),                           &
          int(genomeDstrbMaxSize, kind=bitSetHashKind)  &
        )                                               &
      )

    ! Whenever a collision occurs, increase index and prob the corresponding
    ! element for an empty slot. Do so until an empty slot is found.
    do
      if (genomeDstrb(idx)%genome%isInitialized()) then
        ! Match found
        if (genomeDstrb(idx)%genome == genome) then
          exit
        end if

        idx = modulo(idx + 1, genomeDstrbMaxSize)
        cycle
      end if
      exit
    end do
  end function findGenomeIdx


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getShannonEntropy
  !>  Calculate the unnormalized Shannon entropy
  ! -------------------------------------------------------------------------- !
  function getShannonEntropy() result(entropy)
    integer :: i, genomeIdx, genomeCount
    real :: entropy, probability

    ! Initialize the Shannon diversity index
    entropy = 0.0

    ! Get the corresponding counts of each genomes
    do i = 1, uniqueGenomeCount
      genomeIdx   = genomeIdxArray(i)
      genomeCount = genomeDstrb(genomeIdx)%count

      if (genomeCount == 0) cycle

      probability = real(genomeCount) / real(totalGenomeCount)
      entropy = entropy - probability * log(probability)
    end do
  end function getShannonEntropy


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getRenyiEntropy
  !>  Calculate the Renyi Entropy of order alpha
  !!  a generalization of different entropies including Shannon entropy.
  ! -------------------------------------------------------------------------- !
  function getRenyiEntropy(alpha) result(entropy)
    real, intent(in) :: alpha
    
    real    :: entropy, probabilitySum, probability
    integer :: i, genomeIdx, genomeCount

    real, parameter :: zeroCmpEpsilon = 1e-6
  
    ! Initialize output
    entropy = 0.0
    probabilitySum = 0.0

    ! If alpha is equal to 1.0, use the definition of Shannon entropy
    if (abs(alpha - 1.0) < zeroCmpEpsilon) then
      entropy = getShannonEntropy()
      return
    end if

    do i = 1, uniqueGenomeCount
      genomeIdx   = genomeIdxArray(i)
      genomeCount = genomeDstrb(genomeIdx)%count

      if (genomeCount == 0) cycle

      probability = genomeCount / real(totalGenomeCount)
      probabilitySum = probabilitySum + (probability ** alpha)
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
    if (totalGenomeCount == 0) then
      diversityIdx = 0.0
      return
    end if

    if (present(alpha)) then
      if (isFinite(alpha)) then
        diversityIdx = getRenyiEntropy(alpha)
        return
      end if
    end if

    ! Get the *Normalized* Shannon entropy
    if (totalGenomeCount > 1) then
      diversityIdx = getShannonEntropy() / log(real(totalGenomeCount))
    else
      ! Since we get a 0/0 case when totalGenomeCount = 0, we just set it to 0
      ! to be in line with the behavior for the unnormalized Shannon entropy
      ! (also 0.0 at totalGenomeCount = 0)
      diversityIdx = 0.0
    end if
  end function getDiversityIdx


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getBadGeneDstrb
  !>  Get the distribution of bad genes in the population's genomes.
  ! -------------------------------------------------------------------------- !
  function getBadGeneDstrb() result(badGeneDstrb)
    integer :: badGeneDstrb(MODEL_L)
    integer :: genomeIntArray(MODEL_L)
    logical :: genomeLgclArray(MODEL_L)
    type(StaticBitSet) :: genome
    integer :: genomeCount, i

    badGeneDstrb(:) = 0

    do i = 1, uniqueGenomeCount
      genome      = genomeDstrb(genomeIdxArray(i))%genome
      genomeCount = genomeDstrb(genomeIdxArray(i))%count

      if (genomeCount == 0) cycle

      call extractBitSetData(genome, genomeLgclArray)

      genomeIntArray = logicalToInt(genomeLgclArray)
      badGeneDstrb(:) = badGeneDstrb(:) + (genomeIntArray(:) * genomeCount)
    end do
  end function getBadGeneDstrb


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getUniqueGenomeCount
  !>  Get the number of unique genomes.
  ! -------------------------------------------------------------------------- !
  integer function getUniqueGenomeCount() result(result)
    integer :: i, genomeIdx
    result = 0
    do i = 1, uniqueGenomeCount
      genomeIdx = genomeIdxArray(i)
      if (genomeDstrb(genomeIdx)%count > 0) result = result + 1
    end do
  end function getUniqueGenomeCount


  ! -------------------------------------------------------------------------- !
  ! AGE DISTRIBUTION
  !

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: resetAgeDstrb
  !>  Reset the age distribution.
  ! -------------------------------------------------------------------------- !
  subroutine resetAgeDstrb()
    if (.not.allocated(ageDistribution)) allocate(ageDistribution(0:MODEL_L))
    ageDistribution(:) = 0
  end subroutine resetAgeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateAgeDstrb
  !>  Update the age distribution.
  ! -------------------------------------------------------------------------- !
  subroutine updateAgeDstrb(age)
    integer, intent(in) :: age
      !! The age to be added into the age distribution.

    ageDistribution(age) = ageDistribution(age) + 1
  end subroutine updateAgeDstrb


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocAgeDstrb
  !>  Deallocate age distribution arrays
  ! -------------------------------------------------------------------------- !
  subroutine deallocAgeDstrb()
    if (allocated(ageDistribution)) deallocate(ageDistribution)
  end subroutine deallocAgeDstrb
end module Demographics

module PopulationList
  ! -------------------------------------------------------------------------- !
  ! MODULE:  PopulationList
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing the population list represented as a dynamic array of
  !!  `Person` objects, which in turn represent the individuals in the Penna
  !!  model.
  ! -------------------------------------------------------------------------- !
  use Parameters, only: &
    MODEL_L,        &
    MODEL_M,        &
    MODEL_R,        &
    MODEL_R_MAX,    &
    MODEL_B,        &
    MODEL_V_WEIGHT, &
    MODEL_T,        &
    MODEL_K

  use ErrorMSG, only: raiseError
  use DynamicBitSet, only: BitSet
  use RandNumProcs, only: getRandReal, getRandRange
  use Gene, only: personIK, personRK, GENE_UNHEALTHY, GENE_HEALTHY, getGene
  implicit none
  private

  
  ! `PERSON` DERIVED TYPE
  ! -------------------------------------------------------------------------- !
  type :: Person
    !! A derived type representing individuals in the Penna model.
    integer(kind=personIK) :: genome
      !! Genome of this individual.
    integer :: age
      !! The age of this individual.
    integer :: lifeStat
      !! Vitality of this individual. Values greater than 1 denotes death.

    integer, private :: mutationCount
      !! Number of mutations in this individual's genome.
  end type Person


  ! THE POPULATION ARRAY.
  ! -------------------------------------------------------------------------- !
  type(Person), allocatable :: population(:)
    !! The population array.
  ! logical,      allocatable :: deadPopMask(:)
  type(BitSet) :: deadPopMask
    !! Mask array to filter out dead `Person`s.
  integer :: popArraySize
    !! The size of the population array. The maximum number of `Person`s the
    !! population array can hold.
  integer :: popSize
    !! The actual population size. This should not be confused with
    !! `PopArraySize` which is the size of the population array including
    !! uninitialized `Person` elements.

  integer :: endIdx = 0
    !! The end of the population array in the current time step.
  integer :: futureEndIdx = 0
    !! The end of the population array in between time steps. The actual end.
  integer :: currPersonIdx = 0
    !! The index of the current `Person` being evaluated.

  real, parameter :: GROWTH_FACTOR = 1.5
    !! The threshold at which resizing is triggered.

  ! INDIVIDUAL STATES.
  ! -------------------------------------------------------------------------- !
  integer, parameter :: ALIVE = 0
  integer, parameter :: DEAD_OLD_AGE = 1
  integer, parameter :: DEAD_MUTATION = 2
  integer, parameter :: DEAD_VERHULST = 3

  public :: ALIVE
  public :: DEAD_OLD_AGE
  public :: DEAD_MUTATION
  public :: DEAD_VERHULST

  ! `Person` procedures.
  public :: checkPersonsLife
  public :: checkPersonBirth

  public :: population
  public :: currPersonIdx
  public :: popSize
  protected :: currPersonIdx
  protected :: popSize

  public :: initPopulation
  public :: atEndOfPopulation
  public :: nextPersonIdx
  public :: restartEvalLoop
  public :: freePersonList
contains

  ! -------------------------------------------------------------------------- !
  ! `PERSON` ARRAY.
  ! -------------------------------------------------------------------------- !


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initPopulation
  !>  Initialize the `Person` array with the provided initial conditions:
  !!  `startPopSize` for starting population size; and `initMttnCount` for
  !!  the initial mutation count.
  ! -------------------------------------------------------------------------- !
  subroutine initPopulation(startPopsize, initMttnCount)
    integer, intent(in) :: startPopSize
      !! Starting population size.
    integer, intent(in) :: initMttnCount
      !! Initial mutation count.
    integer :: popIdx

    if (allocated(population)) deallocate(population)
    allocate(population(startPopSize*int(GROWTH_FACTOR)))
    deadPopMask = BitSet()

    ! Initialize each of the `Person`s in the population array.
    popArraySize = startPopSize
    do popIdx = 1, startPopsize
      population(popIdx) = constructPurePerson(initMttnCount)
    end do

    ! Initialize the dead population mask.
    ! NOTE: False means that individuals are alive.
    call deadPopMask%set(.false., 1000)

    ! Initialize the module-wide array pointers/indices.
    currPersonIdx = 1
    endIdx = startPopSize
    futureEndIdx = startPopSize
    popSize = startPopSize
  end subroutine initPopulation


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: constructPurePerson
  !>  Construct and initialize a `Person` object with `mutationCount` number
  !!  of random bad genes.
  ! -------------------------------------------------------------------------- !
  function constructPurePerson(mutationCount) result(new_person)
    integer, intent(in) :: mutationCount
      !! Initial number of mutations.
    type(Person) :: new_person
    
    ! Initialize genome and mutation count.
    new_person % genome = GENE_HEALTHY
    new_person % mutationCount = 0
    call applyInitialMutations(new_person, mutationCount)

    new_person % age = 0
    new_person % lifeStat = ALIVE
  end function constructPurePerson


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: applyInitialMutations
  !>  Apply a set number of mutation to the `Person` object to be initialized.
  ! -------------------------------------------------------------------------- !
  subroutine applyInitialMutations(personObj, mutationCount)
    type(Person), intent(inout) :: personObj
      !! The `Person` object to be modified.
    integer,      intent(in)    :: mutationCount
      !! Number of mutations to apply onto `person_ptr`.

    integer :: mutationIndcs(mutationCount)
    integer :: i

    if (mutationCount > 0) then
      ! Get random indices of genes to mutate.
      mutationIndcs = getRandRange(0, MODEL_L-1, mutationCount)

      ! Apply mutations.
      do i = 1, mutationCount
        personObj % genome = ibset(personObj % genome, mutationIndcs(i))
      end do
    end if
  end subroutine applyInitialMutations


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: atEndOfPopulation
  !>  Inquiry function to check the 
  ! -------------------------------------------------------------------------- !
  logical function atEndOfPopulation()
    atEndOfPopulation = currPersonIdx > endIdx
  end function atEndOfPopulation


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: nextPersonIdx
  !>  Increment the current `Person` index. If the population array is at its
  !!  end for the current time step, `arrStat` will have the value of -1 and the
  !!  `Person` index will not be incremented/, otherwise 0.
  ! -------------------------------------------------------------------------- !
  subroutine nextPersonIdx()
      currPersonIdx = currPersonIdx + 1
  end subroutine nextPersonIdx


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: restartEvalLoop
  !>  Reset the reader pointers/indices. Update the end of the array.
  ! -------------------------------------------------------------------------- !
  subroutine restartEvalLoop()
    call removeDeadPersons()
    endIdx = futureEndIdx
    currPersonIdx = 1

    call deadPopMask%setAll(.false.)
  end subroutine restartEvalLoop


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: removeDeadPersons
  !>  Remove dead individuals as indicated by the logical mask `deadPopMask`
  !!  by overwriting the spaces they occupied with the alive ones.
  ! -------------------------------------------------------------------------- !
  subroutine removeDeadPersons()
    integer :: i
    integer :: deathCount

    ! Remove dead individuals that were evaluated in the current time step.
    deathCount = 0
    do i = 1, endIdx
      if (deadPopMask%get(i)) then
        deathCount = deathCount + 1
      else
        ! "Filter down" alive individuals to occupy the spaces dead ones
        ! occupied.
        population(i - deathCount) = population(i)
      end if
    end do

    ! Continue filtering down alive individuals in the future end section of the
    ! population array. Do nothing if no death occured.
    if (deathCount > 0) then
      do i = endIdx + 1, futureEndIdx
        population(i - deathCount) = population(i)
      end do
    end if

    ! Update the array end indices.
    endIdx = endIdx - deathCount
    futureEndIdx = futureEndIdx - deathCount
  end subroutine removeDeadPersons


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freePersonList
  !>  Free all allocatable arrays.
  ! -------------------------------------------------------------------------- !
  subroutine freePersonList()
    if (allocated(population)) deallocate(population)
  end subroutine freePersonList


  ! -------------------------------------------------------------------------- !
  ! `PERSON`-RELATED PROCEDURES.
  ! -------------------------------------------------------------------------- !


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: person_checkDeath
  !>  Check the death of the `Person` object `self`.
  ! -------------------------------------------------------------------------- ! TODO: Refactor this subroutine.
  subroutine checkPersonsLife(personArr, personIdx)
    type(Person), intent(inout) :: personArr(:)
    integer,      intent(in)    :: personIdx
      !! Population size at the start of the current time step.

    real(kind=personRK) :: verhulstWeight
    real(kind=personRK) :: verhulstFactor
    integer :: nextAge

    nextAge = personArr(personIdx) % age + 1 ! Hypothetical age

    ! ***Death check: Old age
    if (nextAge >= MODEL_L) then
      call deadPopMask%set(.true., personIdx)
      popSize = popSize - 1
      personArr(personIdx) % lifeStat = DEAD_OLD_AGE
      return
    end if

    ! Count mutation.
    if (getGene(personArr(personIdx) % genome, nextAge) == GENE_UNHEALTHY) then
      personArr(personIdx) % mutationCount = &
          personArr(personIdx) % mutationCount + 1
    end if

    ! Get the Verhulst weight for the hypothetical age.
    verhulstWeight = MODEL_V_WEIGHT(nextAge)

    ! ***Death check: Mutation accumulation
    if (personArr(personIdx) % mutationCount >= MODEL_T) then
      personArr(personIdx) % lifeStat = DEAD_MUTATION

    ! ***Death check: Verhulst factor
    else if (verhulstWeight > 0.0_personIK) then
      ! Get Verhulst factor per age.
      verhulstFactor = 1.0 - real(popSize)/real(MODEL_K)*verhulstWeight

      if (getRandReal() > verhulstFactor) then
        personArr(personIdx) % lifeStat = DEAD_VERHULST
      end if
    end if

    ! Mark the current person as dead and for in the next time step.
    if (personArr(personIdx) % lifeStat /= ALIVE) then
      call deadPopMask%set(.true., personIdx)
      popSize = popSize - 1
    end if
  end subroutine checkPersonsLife


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkPersonBirth
  !>  Check for any births event by the `Person` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine checkPersonBirth(personArr, personIdx)
    type(Person), intent(inout) :: personArr(:)
    integer,      intent(in)    :: personIdx
    logical :: gaveBirth
    integer :: i

    ! Check if the current individual can reproduce.
    gaveBirth = (MODEL_R     <= personArr(personIdx) % age .and. &
                 MODEL_R_MAX >= personArr(personIdx) % age)
    if (.not. gaveBirth) return

    do i = 1, MODEL_B
      call addNewPerson(personArr(personIdx) % genome)
    end do

    ! Update the population size.
    popSize = popSize + MODEL_B
  end subroutine checkPersonBirth

  
  ! -------------------------------------------------------------------------- !
  ! FUNCTION: addNewPerson
  !>  Add a `Person` to the population whose genome is inhereted from another
  !!  `Person`.
  ! -------------------------------------------------------------------------- !
  subroutine addNewPerson(genome)
    integer(kind=personIK), intent(in) :: genome
      !! Genome to be inherited by the new `Person`.
    integer :: mutations(MODEL_M)
    integer :: i

    ! Local copy of the parent genome `genome`.                                  TODO: Refactor this subroutine (see adjacent NOTES)
    ! NOTE: Since Fortran is call by reference and `extendPopArray` will mutate
    !       the value of `genome` by deallocating the derived type object it
    !       belongs, `genome` has to be copied locally in this subroutine to 
    !       preserved its value.
    !       This warrants refactoring.
    integer(kind=personIK) :: genomeLocal

    ! Allot a single space for the new `Person` to be added.
    genomeLocal = genome
    if (futureEndIdx == popArraySize) call extendPopArray()
    futureEndIdx = futureEndIdx + 1

    ! Inherit the genome of the new `Person`, then apply random mutations to the
    ! inherited genome.
    ! NOTE: Zero-based indexing for bit sets.
    mutations(:) = 0  ! Initialize `mutations`
    mutations = getRandRange(0, MODEL_L-1, MODEL_M)
    do i = 1, MODEL_M
      ! Flip "gene" bits that are healthy. Do nothing for unhealthy ones.
      if (ibits(genomeLocal, mutations(i), 1) == GENE_HEALTHY) then
        genomeLocal = ibset(genomeLocal, mutations(i))
      end if 
    end do

    population(futureEndIdx) % age = 0
    population(futureEndIdx) % mutationCount = 0
    population(futureEndIdx) % lifeStat = ALIVE
    population(futureEndIdx) % genome = genomeLocal
  end subroutine addNewPerson


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: extendPopArray
  !>  Increase the size of the array by `GROWTH_FACTOR*oldPopSize` where
  !!  `oldPopSize` is the size of the population array that is yet to be
  !!  extended.
  ! -------------------------------------------------------------------------- !
  subroutine extendPopArray()
    integer :: oldPopSize
    integer :: newPopSize
    ! integer :: i

    type(Person), allocatable :: tempPopArray(:)

    if (allocated(tempPopArray)) deallocate(tempPopArray)

    oldPopSize = popArraySize
    newPopSize = int(oldPopSize*GROWTH_FACTOR)

    ! Resize the population array.
    call move_alloc(population, tempPopArray)
    allocate(population(newPopSize))
    population(:oldPopSize) = tempPopArray(:oldPopSize)

    popArraySize = newPopSize

    deallocate(tempPopArray)
  end subroutine extendPopArray
end module PopulationList

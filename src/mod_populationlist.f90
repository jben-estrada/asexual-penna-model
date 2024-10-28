module PopulationList
  ! -------------------------------------------------------------------------- !
  ! MODULE:  PopulationList
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing the population list represented as a dynamic array of
  !!  `Person_t` objects, which in turn represent the individuals in the Penna
  !!  model.
  ! -------------------------------------------------------------------------- !
  use Parameters, only: &
    MODEL_L,            &
    MODEL_M,            &
    MODEL_R,            &
    MODEL_R_MAX,        &
    MODEL_B,            &
    MODEL_T,            &
    MODEL_K,            &
    MODEL_V_WEIGHT,     &
    MODEL_GENOME_MASK

  use ErrorMSG, only: raiseError, raiseWarning
  use CastProcs, only: castIntToChar
  use StaticBitSetType,  only: StaticBitSet
  use Demographics, only: addGenomeToDstrb, delGenomeFromDstrb
  use RandNumProcs, only: getRandReal, getRandRange, getRandInt
  use AbstractPopulation, only: AbstractPopulation_t, AbstractPerson_t
  use Gene, only: GENE_UNHEALTHY, GENE_HEALTHY
  use, intrinsic :: iso_fortran_env, only: personRK => real64
  implicit none
  private


  ! `PERSON_T` DERIVED TYPE
  ! -------------------------------------------------------------------------- !
  type, extends(AbstractPerson_t) :: Person_t
    !! A derived type representing individuals in the Penna model.
    ! integer(kind=personIK) :: genome
    type(StaticBitSet) :: genome
      !! Genome of this individual.
    integer :: age
      !! The age of this individual.
    integer :: lifeStat
      !! Vitality of this individual. Values greater than 1 denotes death.
    integer :: mutationCount
      !! Number of mutations in this individual's genome.
  end type Person_t


  ! Derived type for containing `Person_t` pointers.
  type :: PersonPtr_t
    type(Person_t), pointer :: person => null()
  end type PersonPtr_t


  ! `POPULATION_T` DERIVED TYPE
  ! -------------------------------------------------------------------------- !
  type, extends(AbstractPopulation_t):: Population_t
    private
    type(PersonPtr_t), allocatable :: population(:)
      !! Array holding the individuals in the Penna model.
    type(StaticBitSet)             :: deadPopMask
      !! Mask array to filter out dead `Person_t`s.

    integer :: popArraySize = -1
      !! The size of the population array. The maximum number of `Person_t`s the
      !! population array can hold.
    integer :: popSize = -1
      !! The actual population size. This should not be confused with
      !! `%popArraySize` which is the size of the population array including
      !! uninitialized `Person_t` elements.
    integer :: bornPopSize = 0
      !! Number of born individuals after a time step. Newly born individuals
      !! are to be added at the beginning of the next time step.
    integer :: deadPopSize = 0
      !! Number of dead individuals after a time step.
    integer :: endIdx = 0
      !! The end of the population array in the current time step.
    integer :: futureEndIdx = 0
      !! The end of the population array in between time steps. The actual end
      !! of the dynamic array `%population`.
    integer :: currIdx = 0
      !! The index of the current `Person_t` being evaluated.
    logical, public :: recordGnmDstrb = .false.
      !! Record the genome distribution
  contains
    ! Inquiry functions.
    procedure :: getPopSize => population_getPopSize
      !! Get the population size.
    procedure :: getCurrPerson => population_getCurrPerson
      !! Get the current individual as an `AbstractPerson_t` pointer.
    procedure :: atEndOfPopulation => population_atEndOfPop
      !! Return a logical value of whether the current `Person_t` is the last
      !! individual for the current time step.

    ! Transformational subroutines.
    procedure :: next => population_next
      !! Go to the next `Person_t` object.
    procedure :: startCurrStep => population_startCurrStep
      !! Initialize and begin the current time step. This is where the number
      !! of the newly born individuals from the preceeding time step is added
      !! into the population size
    procedure :: endCurrStep => population_endCurrStep
      !! End the current time step in preparation for the next one. The dead
      !! individuals are removed from the population, and the population array
      !! indices are updated and reset.
    procedure :: evalCurrPerson => population_evalCurrPerson
      !! Evaluate the current individual in the population. The individual is
      !! checked for death event first before incrementing its age and then
      !! checking for birth event.
  
    ! Memory management subroutines.
    procedure :: cleanup => population_cleanup
      !! Deallocate all allocated attributes.
    procedure :: checkPersonPtrAssoc
      !! Check for null `Person` pointers. Prints warning texts if at least one
      !! is encountered.
  end type Population_t


  ! Overload the default constructor of `Population_t`.
  interface Population_t
    module procedure :: population_constructor
  end interface Population_t


  real,    parameter :: GROWTH_FACTOR = 1.5
    !! The threshold at which resizing is triggered.
  logical, parameter :: MASK_ALIVE = .false.
  logical, parameter :: MASK_DEAD = .not.MASK_ALIVE

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

  public :: Population_t
  public :: Person_t
  public :: defaultPersonPtr
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: population_constructor
  !>  `Population_t` constructor
  ! -------------------------------------------------------------------------- !
  function population_constructor(startPopsize, initMttnCount, recordGnmDstrb) &
      result(newPop)
    integer, intent(in) :: startPopSize
      !! Starting population size.
    integer, intent(in) :: initMttnCount
      !! Initial mutation count.
    logical, intent(in) :: recordGnmDstrb
      !! Record genome distribution?
    type(Population_t) :: newPop

    if (allocated(newPop%population)) deallocate(newPop%population)
    allocate(newPop%population(int(startPopSize*GROWTH_FACTOR)))

    newPop%deadPopMask = StaticBitSet(startPopSize, MASK_ALIVE)

    ! Initialize each of the `Person_t`s in the population array.
    newPop%popArraySize = startPopSize
    newPop%population = makePersonPtrArr(startPopSize, initMttnCount)

    if (recordGnmDstrb) then
      ! Initialize the genome distribution
      initGnmDstrb: block
        integer :: i

        do i = 1, startPopsize
          call addGenomeToDstrb(   &
                  newPop%population(i)%person%genome, MODEL_GENOME_MASK  &
              )
        end do
      end block initGnmDstrb
    end if

    ! Initialize the array pointers/indices.
    newPop%currIdx = 1
    newPop%endIdx = startPopSize
    newPop%futureEndIdx = startPopSize
    newPop%popSize = startPopSize
    newPop%recordGnmDstrb = recordGnmDstrb
  end function population_constructor


  ! -------------------------------------------------------------------------- !
  ! `PERSON` ARRAY.
  ! -------------------------------------------------------------------------- !


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: makePersonPtrArr
  !>  Make an array of `PersonPtr_t` with `initMttnCount` number of mutations
  !!  per individual.
  ! -------------------------------------------------------------------------- !
  function makePersonPtrArr(size, initMttnCount) result(personPtrArr)
    integer, intent(in) :: size
    integer, intent(in) :: initMttnCount

    type(PersonPtr_t), allocatable :: personPtrArr(:)
    type(StaticBitSet) :: pureGenome
    integer :: i

    if (allocated(personPtrArr)) deallocate(personPtrArr)
    allocate(personPtrArr(size))

    pureGenome = StaticBitSet(MODEL_L, GENE_HEALTHY)

    do i = 1, size
      if (associated(personPtrArr(i)%person)) personPtrArr(i)%person => null()
      allocate(personPtrArr(i)%person)
      call initNewPerson(personPtrArr(i)%person, pureGenome, initMttnCount)
    end do
  end function makePersonPtrArr


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: population_getPopSize
  !>  Get the population size.
  ! -------------------------------------------------------------------------- !
  integer function population_getPopSize(self)
    class(Population_t), intent(in) :: self
    population_getPopSize = self%popSize
  end function population_getPopSize


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: population_getCurrPerson
  !>  Get the current person in the population as an `AbstractPerson_t` pointer.
  ! -------------------------------------------------------------------------- !
  function population_getCurrPerson(self) result(personObj_ptr)
    class(Population_t), intent(in) :: self
    class(AbstractPerson_t), pointer :: personObj_ptr
    personObj_ptr => self%population(self%currIdx)%person
  end function population_getCurrPerson


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: population_atEndOfPop
  !>  Return a logical value of whether the current `Person_t` is the last
  !!  individual for the current time step.
  ! -------------------------------------------------------------------------- !
  logical function population_atEndOfPop(self)
    class(Population_t), intent(in) :: self
    population_atEndOfPop = (self%currIdx > self%endIdx)
  end function population_atEndOfPop


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: population_startCurrStep
  !>  Initialize the current time step
  ! -------------------------------------------------------------------------- !
  subroutine population_startCurrStep(self)
    class(Population_t), intent(inout) :: self

    self%popSize = self%popSize + self%bornPopSize
    self%bornPopSize = 0
  end subroutine population_startCurrStep


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: population_next
  !>  Go to the next person in the population.
  ! -------------------------------------------------------------------------- !
  subroutine population_next(self)
    class(Population_t), intent(inout) :: self
    self%currIdx = self%currIdx + 1
  end subroutine population_next


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: population_endCurrStep
  !>  End the current time step to prepare for the next time step. The
  !!  population indices are reset, and the dead individuals are remvoed from
  !!  the population array.
  ! -------------------------------------------------------------------------- !
  subroutine population_endCurrStep(self)
    class(Population_t), intent(inout) :: self
    integer :: deadPopMaskCurrSize

    call removeDeadPersons(self)
    self%endIdx = self%futureEndIdx
    self%currIdx = 1

    self%popSize = self%popSize - self%deadPopSize
    self%deadPopSize = 0

    ! Expand the dead population mask if it gets full. The mask does not
    ! contract in size with the population size.
    deadPopMaskCurrSize = self%deadPopMask%getSize()
    if (self%futureEndIdx > deadPopMaskCurrSize) then
      call self%deadPopMask%changeSize(int(self%futureEndIdx * GROWTH_FACTOR))
    end if

    if (self%futureEndIdx > 0) then
      call self%deadPopMask%set(MASK_ALIVE, 1, self%futureEndIdx)
    end if
  end subroutine population_endCurrStep


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: removeDeadPersons
  !>  Remove dead individuals as indicated by the logical mask `deadPopMask`
  !!  by overwriting the spaces they occupied with the alive ones.
  ! -------------------------------------------------------------------------- !
  subroutine removeDeadPersons(self)
    class(Population_t), intent(inout) :: self
    integer :: i
    integer :: deathCount
    logical :: currPersonIsDead

    ! Remove dead individuals that were evaluated in the current time step.
    deathCount = 0
    do i = 1, self%futureEndIdx
      currPersonIsDead = .false.
      if (i <= self%endIdx) then
        currPersonIsDead = self%deadPopMask%get(i) .eqv. MASK_DEAD

        if (currPersonIsDead) then
          deathCount = deathCount + 1
          call freePersonPtr(self%population(i))
        end if
      end if

      ! "Filter down" live individuals to occupy the spaces dead ones once 
      ! occupied.
      if (deathCount > 0 .and. .not.currPersonIsDead) then
        if (i - deathCount >= 1) then
          self%population(i - deathCount)%person => self%population(i)%person
        end if
        self%population(i)%person => null()
      end if
    end do

    ! Update the array end indices.
    self%endIdx       = self%endIdx       - deathCount
    self%futureEndIdx = self%futureEndIdx - deathCount
  end subroutine removeDeadPersons


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freePersonList
  !>  Free all allocatable arrays.
  ! -------------------------------------------------------------------------- !
  subroutine population_cleanup(self)
    class(Population_t), intent(inout) :: self
    integer :: i

    if (allocated(self%population)) then
        do i = lbound(self%population, 1), ubound(self%population, 1)
          call freePersonPtr(self%population(i))
        end do

       deallocate(self%population)
    end if
  end subroutine population_cleanup


  ! -------------------------------------------------------------------------- !
  ! `PERSON`-RELATED PROCEDURES.
  ! -------------------------------------------------------------------------- !


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initNewPerson
  !>  Initialize a new `Person_t` object.
  ! -------------------------------------------------------------------------- !
  subroutine initNewPerson(person, genome, mutationCount)
    type(Person_t),          intent(inout) :: person
    type(StaticBitSet),      intent(in)    :: genome
    integer,                 intent(in)    :: mutationCount
  
    ! Initialize genome and mutation count.
    person%genome = genome
    person%mutationCount = 0
    call applyInitialMutations(person, mutationCount)

    person%age = 0
    person%lifeStat = ALIVE
  end subroutine initNewPerson


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: applyInitialMutations
  !>  Apply a set number of mutation to the `Person_t` object to be initialized.
  ! -------------------------------------------------------------------------- !
  subroutine applyInitialMutations(person, mutationCount)
    type(Person_t), intent(inout) :: person
      !! The `Person_t` object to be modified.
    integer,      intent(in)    :: mutationCount
      !! Number of mutations to apply onto `person_ptr`.

    integer, allocatable :: mutationIndcs(:)
    integer :: mutationCount_lcl
    integer :: i

    if (mutationCount >= 0) then
      mutationCount_lcl = mutationCount
    else
      ! For the case MTTN_COUNT = -1, randomize the mutation count of the 
      mutationCount_lcl = getRandInt(0, MODEL_L)
    end if

    if (mutationCount_lcl > 0) then
      allocate(mutationIndcs(mutationCount_lcl))

      ! Get random indices of genes to mutate.
      mutationIndcs = getRandRange(1, MODEL_L, mutationCount_lcl)

      ! Apply mutations.
      do i = 1, mutationCount_lcl
        call person%genome%set(  &
            GENE_UNHEALTHY, mutationIndcs(i), mutationIndcs(i)  &
          )
      end do

      deallocate(mutationIndcs)
    end if
  end subroutine applyInitialMutations


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: population_evalCurrPerson
  !>  Evaluate the current person in the population. Death event is checked
  !!  first, before age increment and birth event are checked.
  ! -------------------------------------------------------------------------- !
  subroutine population_evalCurrPerson(self)
    class(Population_t), intent(inout) :: self

    real(kind=personRK) :: verhulstWeight
    real(kind=personRK) :: verhulstFactor
    integer :: nextAge
    logical :: isDead

    associate(currPerson => self%population(self%currIdx)%person)
      nextAge = currPerson%age + 1
      isDead = .false.
  
      ! ***Death check: Old age
      if (nextAge > MODEL_L) then
        currPerson%lifeStat = DEAD_OLD_AGE
        isDead = .true.
      else
        ! Count mutation.
        if (currPerson%genome%get(nextAge) .eqv. GENE_UNHEALTHY) then
            currPerson%mutationCount = currPerson%mutationCount + 1
        end if
  
        verhulstWeight = MODEL_V_WEIGHT(nextAge)
  
        ! ***Death check: Mutation
        if (currPerson%mutationCount >= MODEL_T) then
          currPerson%lifeStat = DEAD_MUTATION
          isDead = .true.
        
        ! ***Death check: Verhulst weight
        else if (verhulstWeight > 0.0_personRK) then
          ! Get Verhulst factor per age.
          verhulstFactor = &
              (1.0_personRK - real(self%popSize, kind=personRK) / &
               real(MODEL_K, kind=personRK)*verhulstWeight)
  
          if (getRandReal() > verhulstFactor) then
            currPerson%lifeStat = DEAD_VERHULST
            isDead = .true.
          end if
        end if
      end if

      if (isDead) then
        self%deadPopSize = self%deadPopSize + 1
        call self%deadPopMask%set(MASK_DEAD, self%currIdx, self%currIdx)

        if (self%recordGnmDstrb) then
          call delGenomeFromDstrb(currPerson%genome, MODEL_GENOME_MASK)
        end if
      else
        currPerson%age = nextAge
        call checkPersonBirth(self)
      end if
    end associate
  end subroutine population_evalCurrPerson


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkPersonBirth
  !>  Check for any births event by the current `Person_t` object of `popObj`.
  ! -------------------------------------------------------------------------- !
  subroutine checkPersonBirth(self)
    class(Population_t), intent(inout) :: self
    logical :: gaveBirth
    integer :: i

    associate(currPerson => self%population(self%currIdx)%person)
      ! Check if the current individual can reproduce.
      gaveBirth = (MODEL_R <= currPerson%age .and. &
                   MODEL_R_MAX >= currPerson%age)

      ! Add newly born individuals to the population.
      if (gaveBirth) then
        do i = 1, MODEL_B
          ! Extend the population array if needed.
          if (self%futureEndIdx == self%popArraySize) then
            call extendPopArray(self)
          end if
          self%futureEndIdx = self%futureEndIdx + 1

          ! Allocate and initialize a new `Person_t`.
          associate( &
                newPersonPtr => self%population(self%futureEndIdx) &
              )

            call freePersonPtr(newPersonPtr)
            allocate(newPersonPtr%person)
            call initNewPerson(newPersonPtr%person, currPerson%genome, MODEL_M)

            if (self%recordGnmDstrb) then
              call addGenomeToDstrb(   &
                      newPersonPtr%person%genome, MODEL_GENOME_MASK  &
                  )
            end if
          end associate
        end do

        self%bornPopSize = self%bornPopSize + MODEL_B
      end if
    end associate
  end subroutine checkPersonBirth


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freePersonPtr
  !>  Free `Person_t` pointer.
  ! -------------------------------------------------------------------------- !
  subroutine freePersonPtr(personPtrObj)
    type(PersonPtr_t), intent(inout) :: personPtrObj
    if (associated(personPtrObj%person)) deallocate(personPtrObj%person)
    personPtrObj%person => null()
  end subroutine freePersonPtr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: extendPopArray
  !>  Increase the size of the array by `GROWTH_FACTOR*oldPopSize` where
  !!  `oldPopSize` is the size of the population array that is yet to be
  !!  extended.
  ! -------------------------------------------------------------------------- !
  subroutine extendPopArray(self)
    class(Population_t), intent(inout) :: self
    integer :: oldPopArrSize
    integer :: newPopArrSize

    type(PersonPtr_t), allocatable :: tempPopArray(:)

    if (allocated(tempPopArray)) deallocate(tempPopArray)

    oldPopArrSize = self%popArraySize
    newPopArrSize = int((oldPopArrSize + 1)*GROWTH_FACTOR)

    ! Resize the population array.
    call move_alloc(self%population, tempPopArray)
    allocate(self%population(newPopArrSize))
    self%population(:oldPopArrSize) = tempPopArray(:oldPopArrSize)

    self%popArraySize = newPopArrSize

    deallocate(tempPopArray)
  end subroutine extendPopArray


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: defaultPersonPtr
  !>  Return a `AbstractPerson_t` pointer as is if its data type is `Person_t`
  !!  Otherwise, return a null pointer.
  ! -------------------------------------------------------------------------- !
  function defaultPersonPtr(abstractPerson_ptr) result(person_ptr)
    class(AbstractPerson_t), pointer, intent(in) :: abstractPerson_ptr
    type(Person_t), pointer :: person_ptr
    
    select type(abstractPerson_ptr)
    type is (Person_t)
      person_ptr => abstractPerson_ptr
    class default
      person_ptr => null()
    end select
  end function defaultPersonPtr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkPersonPtrAssoc
  !>  Check the whether `Person` pointers in the active population array are
  !!  associated or not. Prints warning text if at least one pointer is null.
  ! -------------------------------------------------------------------------- !
  subroutine checkPersonPtrAssoc(self)
    class(Population_t), intent(in) :: self
    integer :: i

    do i = 1, self%endIdx
      if (.not. associated(self%population(i)%person)) then
        call raiseWarning("Person #" // castIntToChar(i) // " is null.", &
            stopProgram=.false.)
      end if
    end do
  end subroutine checkPersonPtrAssoc
end module PopulationList

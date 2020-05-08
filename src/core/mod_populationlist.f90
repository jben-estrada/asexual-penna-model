module PopulationList
  ! -------------------------------------------------------------------------- !
  ! MODULE:  PopulationList
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing an implementation of singly linked-list of `Person`
  !!  objects for representing the population in Penna model simulations.
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
  use RandNumProcs, only: getRandReal, getRandRange
  use Gene, only: personIK, personRK, GENE_UNHEALTHY, GENE_HEALTHY, getGene
  implicit none
  private

  
  ! `PERSON` DERIVED TYPE
  ! -------------------------------------------------------------------------- !
  type :: Person
    !! A node to the population linked-list. This represents a single
    !! individual with its own age and genome.
    private
    integer(kind=personIK) :: genome
      !! Genome of this individual.
    integer :: age
      !! The age of this individual.
    integer :: lifeStat
      !! Vitality of this individual. Values greater than 1 denotes death.
    integer :: mutationCount
      !! Number of mutations in this individual's genome.

    type(Person), pointer  :: next => null()
      !! Pointer to the next element in the list of individuals.
  contains
    procedure :: checkDeath => person_checkDeath
      !! Check if this `Person` object is to die in the current time step.
    procedure :: checkBirth => person_checkBirth
      !! Check if this `Person` object is to reproduce in the current time step.
    procedure :: getLifeStat => person_getLifeStat
      !! Get the life status of this `Person` object.
    procedure :: getGenome => person_getGenome
      !! Get the genome of this `Person` object.
    procedure :: getAge => person_getAge
      !! Get the age of this `Person` object.
    procedure :: incrementAge => person_incrementAge
      !! Increment the age of this `Person` object.
  end type Person


  ! `PERSON` LINKED-LIST POINTERS.
  ! -------------------------------------------------------------------------- !
  ! List pointers.
  type(Person), pointer :: head_ptr => null()
    !! The head of the list.
  type(Person), pointer :: tail_ptr => null()
    !! The tail of the list at the beginning of each time step. 
  type(Person), pointer :: futureTail_ptr => null()
    !! The actual tail of the list.

  ! Reader pointers.
  type(Person), pointer :: readerCurr_ptr => null()
    !! The reading pointer for reading and modifying the list.
    !! This points to the current individual.
  type(Person), pointer :: readerPrev_ptr => null()
    !! The pointer before `readerCurr_ptr`.

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

  public :: Person
  public :: initPersonList
  public :: goToNextPerson
  public :: restartReadingList
  public :: getCurrPerson
  public :: freePersonList
  public :: countElem
contains

  ! -------------------------------------------------------------------------- !
  ! `PERSON` LINKED-LIST INITIALIZATION.
  ! -------------------------------------------------------------------------- !


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initPersonList
  !>  Initialize the `Person` linked list with the provided initial conditions:
  !!  `startPopSize` for starting population size; and `initMttnCount` for
  !!  the initial mutation count.
  ! -------------------------------------------------------------------------- !
  subroutine initPersonList(startPopsize, initMttnCount)
    integer, intent(in) :: startPopSize
      !! Starting population size.
    integer, intent(in) :: initMttnCount
      !! Initial mutation count.

    type(Person), pointer :: newPerson_ptr
    type(Person), pointer :: oldPerson_ptr
    integer :: i

    ! Initialize the head of the `Person` list.
    head_ptr => constructPurePerson(initMttnCount)

    ! Append the new individuals.
    oldPerson_ptr => head_ptr
    newPerson_ptr => null()
    if (startPopSize == 1) then
      tail_ptr => head_ptr
    else
      do i = 1, startPopSize - 1
        newPerson_ptr => constructPurePerson(initMttnCount)

        ! Append the new `Person` to the list.
        oldPerson_ptr % next => newPerson_ptr
        oldPerson_ptr => newPerson_ptr
      end do
      
      tail_ptr => newPerson_ptr
    end if

    futureTail_ptr => tail_ptr

    ! Initialize the reader pointer.
    readerCurr_ptr => head_ptr
    readerPrev_ptr => null()
  end subroutine initPersonList


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: constructPurePerson
  !>  Construct and initialize a `Person` object with `mutationCount` number
  !!  of random bad genes.
  ! -------------------------------------------------------------------------- !
  function constructPurePerson(mutationCount) result(person_ptr)
    integer, intent(in) :: mutationCount
      !! Initial number of mutations.
    type(Person), pointer :: person_ptr

    allocate(person_ptr)
    
    ! Initialize genome and mutation count.
    person_ptr % genome = GENE_HEALTHY
    person_ptr % mutationCount = 0
    call applyInitialMutations(person_ptr, mutationCount)

    person_ptr % age = 0
    person_ptr % lifeStat = ALIVE
  end function constructPurePerson


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: applyInitialMutations
  !>  Apply a set number of mutation to a `Person` object to be initialized.
  ! -------------------------------------------------------------------------- !
  subroutine applyInitialMutations(person_ptr, mutationCount)
    type(Person), pointer, intent(inout) :: person_ptr
      !! The pointer to the individual or the `Person` object to be initialized.
    integer,               intent(in)    :: mutationCount
      !! Number of mutations to apply onto `person_ptr`.

    integer :: mutationIndcs(mutationCount)
    integer :: i

    if (mutationCount > 0) then
      ! Get random indices of genes to mutate.
      mutationIndcs = getRandRange(1, MODEL_L, mutationCount)

      ! Apply mutations.
      do i = 1, mutationCount
        person_ptr % genome = ior(person_ptr % genome, &
            int(shiftl(1, mutationIndcs(i) - 1), kind=personIK))
      end do
    end if
  end subroutine applyInitialMutations


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: goToNextPerson
  !>  Go to the next `Person` object in the `Person` linked-list. This also
  !!  updates the age of previously checked `Person` object or remove them when
  !!  it is marked as dead.
  ! -------------------------------------------------------------------------- !
  subroutine goToNextPerson(listStat)
    integer, intent(out) :: listStat
      !! List status. Returns -1 if the current element would be at the tail of
      !! list after incrementing the pointer. Otherwise, return 0.

    ! Check the position of the new current `Person` object in the list.
    if (associated(readerCurr_ptr, tail_ptr)) then
      ! NOTE: Removing the current `Person` object moves the `readerCurr_ptr`
      ! to its next element.
      if (readerCurr_ptr % lifeStat /= ALIVE) call removeCurrPerson()
      listStat = -1
    else
      if (readerCurr_ptr % lifeStat == ALIVE) then
        ! Go to the next `Person` element.
        readerPrev_ptr => readerCurr_ptr
        readerCurr_ptr => readerCurr_ptr % next

        listStat = 0
      else
        if (associated(readerCurr_ptr, tail_ptr)) then
          listStat = -1
        else
          listStat = 0
        end if

        call removeCurrPerson()
      end if
    end if
  end subroutine goToNextPerson


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: removeCurrPerson
  !>  Remove the current `Person` object, i.e. the object `readerCurr_ptr` is
  !!  pointing at.
  ! -------------------------------------------------------------------------- !
  subroutine removeCurrPerson()
    type(Person), pointer :: toBeRemoved_ptr

    ! Mark the current pointer as smt to be freed.
    toBeRemoved_ptr => readerCurr_ptr

    ! Edge case: head of the list. The next element becomes the head.
    if (associated(readerCurr_ptr, head_ptr)) then
      head_ptr => readerCurr_ptr % next
      readerCurr_ptr => readerCurr_ptr % next
    ! Edge case: delayed tail of the list. The previous element becomes the tail
    else if (associated(readerCurr_ptr, tail_ptr)) then
      ! Update the actual tail pointer.
      if (associated(futureTail_ptr, tail_ptr)) then
        futureTail_ptr => readerPrev_ptr
        futureTail_ptr % next => null()
      end if

      
      ! Move the tail back one element.
      tail_ptr => readerPrev_ptr
      ! Update the link of the (delayed) tail to the next element.
      tail_ptr % next => readerCurr_ptr % next
      ! Move the current pointer back one element.
      readerCurr_ptr => readerPrev_ptr

      ! NOTE: We do not care about the previous pointer as the list to be read
      ! at the current time step is already at the end.
      readerPrev_ptr => null()
    ! General case.
    else
      readerPrev_ptr % next => readerCurr_ptr % next
      readerCurr_ptr => readerCurr_ptr % next
    end if
    
    deallocate(toBeRemoved_ptr)
  end subroutine removeCurrPerson


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: restartReadingList
  !>  Reset the reader pointers. Update the tail of the list.
  ! -------------------------------------------------------------------------- !
  subroutine restartReadingList()
    readerCurr_ptr => head_ptr
    readerPrev_ptr => null()

    tail_ptr => futureTail_ptr
  end subroutine restartReadingList


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCurrPerson
  !>  Get the pointer pointing at current `Person` object, i.e. the 
  !   `readerCurr_ptr`.
  ! -------------------------------------------------------------------------- !
  function getCurrPerson() result(person_ptr)
    type(Person), pointer :: person_ptr
    
    person_ptr => readerCurr_ptr
  end function getCurrPerson


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freePersonList
  !>  Free all the allocated elements of the `Person` linked-list.
  ! -------------------------------------------------------------------------- !
  subroutine freePersonList(popSize)
    integer, intent(in) :: popSize
      !! Population size.
    type(Person), pointer :: currPerson_ptr
    type(Person), pointer :: toBeFreed_ptr

    ! Prevent doubling freeing.
    if (popSize == 0 .and. associated(head_ptr)) currPerson_ptr => null()

    ! Deallocate all the elements of th `Person` linked-list.
    currPerson_ptr => head_ptr
    do
      if (associated(currPerson_ptr)) then
        toBeFreed_ptr => currPerson_ptr
        currPerson_ptr => currPerson_ptr % next

        deallocate(toBeFreed_ptr)
      else
        exit
      end if
    end do

    ! Nullify all pointers in this module.
    readerCurr_ptr => null()
    readerPrev_ptr => null()
    head_ptr => null()
    tail_ptr => null()
    futureTail_ptr => null()
  end subroutine freePersonList


  ! -------------------------------------------------------------------------- !
  ! `PERSON` BOUND PROCEDURES.
  ! -------------------------------------------------------------------------- !


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: person_checkDeath
  !>  Check the death of the `Person` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine person_checkDeath(self, popSize)
    class(Person), intent(inout) :: self
    integer,       intent(in)    :: popSize
      !! Population size at the start of the current time step.

    real(kind=personRK) :: verhulstWeight
    real(kind=personRK) :: verhulstFactor
    integer :: nextAge

    nextAge = self % age + 1 ! Hypothetical age

    ! ***Death check: Old age
    if (nextAge >= MODEL_L) then
      self % lifeStat = DEAD_OLD_AGE
      return
    end if

    ! Count mutation.
    if (getGene(self % genome, nextAge) == GENE_UNHEALTHY) then
      self % mutationCount = self % mutationCount + 1
    end if

    ! Get the Verhulst weight for the hypothetical age.
    verhulstWeight = MODEL_V_WEIGHT(nextAge)

    ! ***Death check: Mutation accumulation
    if (self % mutationCount >= MODEL_T) then
      self % lifeStat = DEAD_MUTATION

    ! ***Death check: Verhulst factor
    else if (verhulstWeight > 0.0_personIK) then
      ! Get Verhulst factor per age.
      verhulstFactor = 1.0 - real(popSize)/real(MODEL_K)*verhulstWeight

      if (getRandReal() > verhulstFactor) then
        self % lifeStat = DEAD_VERHULST
      end if
    end if
  end subroutine person_checkDeath


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: person_checkBirth
  !>  Check for any births event by the `Person` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine person_checkBirth(self, hashBirth)
    class(Person), intent(inout) :: self
    logical,       intent(out)   :: hashBirth
      !! True if a birth event occurs. Otherwise, false.

    type(Person), pointer :: newPerson_ptr
    type(Person), pointer :: prevPerson_ptr
    integer :: i

    ! Check if the current individual can reproduce.
    hashBirth = MODEL_R <= self % age .and. self % age <= MODEL_R_MAX
    if (.not. hashBirth) return

    ! Add new born individuals to the next generation.
    prevPerson_ptr => futureTail_ptr
    newPerson_ptr => null()
    do i = 1, MODEL_B
      ! Pass the genome of the current individual to the newborn ones.
      newPerson_ptr => constructPerson(self % genome)

      ! Append the new `Person` object at the end of the list.
      prevPerson_ptr % next => newPerson_ptr
      prevPerson_ptr => newPerson_ptr
    end do

    ! Update the tail of the list which must be the latest added `Person`
    ! object.
    if (MODEL_B > 0) futureTail_ptr => newPerson_ptr
  end subroutine person_checkBirth


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: constructPerson
  !>  Construct and initialize a `Person` object whose genome is inhereted
  !!  from another `Person` object.
  ! -------------------------------------------------------------------------- !
  function constructPerson(genome) result(person_ptr)
    integer(kind=personIK), intent(in) :: genome
      !! Genome from another `Person` object.    
    type(Person), pointer :: person_ptr
      !! Newly minted `Person` object.

    integer :: mutations(MODEL_M)
    integer :: i

    allocate(person_ptr)

    mutations(:) = 0  ! Initialize `mutations`
    mutations = getRandRange(1, MODEL_L, MODEL_M)

    person_ptr % genome = genome
    do i = 1, size(mutations)
      if (getGene(person_ptr % genome, mutations(i)) == GENE_HEALTHY) then
        person_ptr % genome = ior(person_ptr % genome, &
            shiftl(1_personIK, mutations(i) - 1))
      end if
    end do

    person_ptr % age = 0
    person_ptr % mutationCount = 0
    person_ptr % lifeStat = ALIVE
  end function constructPerson

  ! -------------------------------------------------------------------------- !
  ! FUNCTION: person_getLifeStat
  !>  Get the life status of the `Person` object `self`.
  ! -------------------------------------------------------------------------- !
  integer function person_getLifeStat(self)
    class(Person), intent(in) :: self
    
    person_getLifeStat = self % lifeStat
  end function person_getLifeStat


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: person_getGenome
  !>  Get the genome of the `Person` object `self`.
  ! -------------------------------------------------------------------------- !
  integer(kind=personIK) function person_getGenome(self)
    class(Person), intent(in) :: self

    person_getGenome = self % genome
  end function person_getGenome


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: person_getAge
  !>  Get the age of the `Person` object `self`.
  ! -------------------------------------------------------------------------- !
  integer function person_getAge(self)
    class(Person), intent(in) :: self
    
    person_getAge = self % age
  end function person_getAge


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: person_incrementAge
  !>  Increment age of the `Person` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine person_incrementAge(self)
    class(Person), intent(inout) :: self

    self % age = self % age + 1
  end subroutine person_incrementAge


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: person_incrementAge
  !>  Count the actual elements of the `Person` linked-list. Should only be
  !!  used for debugging.
  ! -------------------------------------------------------------------------- !
  integer function countElem()
    type(Person), pointer :: currPerson_ptr

    countElem = 0
    currPerson_ptr => head_ptr
    do
      if (associated(currPerson_ptr)) then
        countElem = countElem + 1
        currPerson_ptr => currPerson_ptr % next
      else
        exit
      end if
    end do
  end function
end module PopulationList

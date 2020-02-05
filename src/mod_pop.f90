module Pop
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Pop
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing evaluation and generation of population
  ! -------------------------------------------------------------------------- !
  use Gene
  use ModelParam
  implicit none
  private

  type :: Person
    !! A node to the population linked-list. This represents a single
    !! individual with its own age and genome.
    integer(kind=personIK) :: genome
      !! Genome of this individual.
    integer :: age
      !! The age of this individual.
    integer :: deathIndex
      !! Vitality of this individual. Values greater than 1 denotes death.
    integer :: mutationCount
      !! Number of mutations in this individual's genome.

    type(Person), pointer  :: next => null()
      !! Pointer to the next element in the list of individuals.
  end type Person

  type, public :: PersonList
    !! Linked-list of `Person` objects.
    private
    type(Person), pointer :: head_ptr => null()
      !! The head of the list.
    type(Person), pointer :: tail_ptr => null()
      !! The tail of the list at every beginning of each time steps. 
    type(Person), pointer :: newTail_ptr => null()
      !! The transient tail of the list during each time steps. At the end of 
      !! each time steps, `tail_ptr` is updated to point to this pointer.

    type(Person), pointer :: current_ptr => null()
      !! The reading pointer for reading and modifying the list.
      !! This points to the current individual.
    type(Person), pointer :: old_ptr => null()
      !! The pointer before `current_ptr`.
  contains
    ! Inquiry procedures.
    procedure :: isCurrIndivDead
    procedure :: isCurrIndivMature
    procedure :: getCurrIndivAge
    procedure :: getCurrIndivGenome
    procedure :: determineDeathType

    ! Transformational procedures.
    procedure :: killCurrentIndiv
    procedure :: checkCurrIndivDeath
    procedure :: updateCurrIndivAge
    procedure :: reproduceCurrIndiv
    procedure :: nextElem

    ! Subroutines for memory management.
    procedure :: resetReadPtrs
    procedure :: freePtr
  end type PersonList

  ! -------------------------------------------------------------------------- !
  ! Individual states.
  integer, public, parameter :: ALIVE = 1
  integer, public, parameter :: DEAD_OLD_AGE = 2
  integer, public, parameter :: DEAD_MUTATION = 3
  integer, public, parameter :: DEAD_VERHULST = 4
  integer, public, parameter :: DEATH_REASONS(*) = &
      [ALIVE,        &
      DEAD_OLD_AGE,  &
      DEAD_MUTATION, &
      DEAD_VERHULST]

  public :: constructPersonList
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: constructPersonList
  !>  Initialize a `PersonList` object.
  ! -------------------------------------------------------------------------- !
  function constructPersonList(startPopSize, initMttnCount) result(newLL)
    integer, intent(in) :: startPopSize
      !! Starting population size.
    integer, intent(in) :: initMttnCount
      !! Initial mutation count.

    type(PersonList)    :: newLL
    
    allocate(newLL % head_ptr)
    call generatePopulation(newLL, startPopSize, initMttnCount)
    newLL % newTail_ptr => newLL % tail_ptr
    newLL % current_ptr => newLL % head_ptr
  end function constructPersonList


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: determineDeathType
  !>  Determine the death of the current individual. This routine fails if
  !!  the current individual is alive.
  ! -------------------------------------------------------------------------- !
  subroutine determineDeathType(self, deathByAge, deathByMutation, &
        deathByVerhulst)

    class(PersonList), intent(in)   :: self
      !! The linked-list of individuals represented as `Person` objects.
    integer, pointer, intent(inout) :: deathByAge
      !! Pointer for death by age counter.
    integer, pointer, intent(inout) :: deathByMutation
      !! Pointer for death by mutation.
    integer, pointer, intent(inout) :: deathByVerhulst
      !! Pointer for death by Verhulst killing.

    select case (self % current_ptr % deathIndex)
      case (DEAD_OLD_AGE)
        deathByAge = deathByAge + 1

      case (DEAD_MUTATION)
        deathByMutation = deathByMutation + 1

      case (DEAD_VERHULST)
        deathByVerhulst = deathByVerhulst + 1

      case default
        print "(a)", "***ERROR. The current individual is not dead " // &
            "or has an invalid death index."
        error stop
    end select
  end subroutine determineDeathType


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeHealthyIndiv
  !>  Initialize the `Person` object `indiv_ptr` is pointing to `Person` with 
  !!  a healthy genome by default. Can be made to have the `Person` object to
  !!  have initial number of bad genes.
  ! -------------------------------------------------------------------------- !
  subroutine initializeHealthyIndiv(indiv_ptr, mutationCount)
    type(Person), pointer, intent(inout) :: indiv_ptr
      !! The pointer to the individual or the `Person` object to be initialized.
    integer,               intent(in)    :: mutationCount
      !! Initial number of mutations.

    ! Initialize genome and mutation count.
    indiv_ptr % genome = GENE_HEALTHY
    indiv_ptr % mutationCount = mutationCount
    call applyInitialMutations(indiv_ptr, mutationCount)

    indiv_ptr % age = 0
    indiv_ptr % deathIndex = ALIVE
  end subroutine initializeHealthyIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: applyInitialMutations
  !>  Apply a set number of mutation to a `Person` object to be initialized.
  ! -------------------------------------------------------------------------- !
  subroutine applyInitialMutations(indiv_ptr, mutationCount)
    use RandInd, only: generateIndices

    type(Person), pointer, intent(inout) :: indiv_ptr
      !! The pointer to the individual or the `Person` object to be initialized.
    integer,               intent(in)    :: mutationCount
      !! Number of mutations to apply onto `indiv_ptr`.

    integer, allocatable :: mutationIndcs(:)
    integer :: i

    if (mutationCount > 0) then
      ! Get random indices of genes to mutate.
      allocate(mutationIndcs(mutationCount))
      call generateIndices(0, MODEL_L - 1, mutationIndcs)

      ! Apply mutations.
      do i = 1, mutationCount
        indiv_ptr % genome = ior(indiv_ptr % genome, &
            int(shiftl(1, mutationIndcs(i)), kind=personIK))
      end do

      deallocate(mutationIndcs)
    end if
  end subroutine applyInitialMutations


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeIndiv
  !>  Initialize `indiv_ptr` with genes inherited from another `Person`
  !!  object.
  ! -------------------------------------------------------------------------- !
  subroutine initializeIndiv(indiv_ptr, genome)
    use RandInd, only: generateIndices

    type(Person), pointer,  intent(inout) :: indiv_ptr
      !! The pointer to the individual or the `Person` object to be initialized.
    integer(kind=personIK), intent(in)    :: genome
      !! Genome from another `Person` object.

    integer :: mutations(MODEL_M)
    integer :: i

    mutations(:) = 0  ! Initialize `mutations`
    call generateIndices(0, MODEL_L - 1, mutations)

    indiv_ptr % genome = genome
    do i = 1, size(mutations)
      if (getGene(indiv_ptr % genome, mutations(i)) == GENE_HEALTHY) then
        indiv_ptr % genome = ior(indiv_ptr % genome, &
            int(shiftl(1, mutations(i)), kind=personIK))
      end if
    end do

    indiv_ptr % age = 0
    indiv_ptr % mutationCount = 0
    indiv_ptr % deathIndex = ALIVE
  end subroutine initializeIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: generatePopulation
  !>  Generate a list of population of `startPopSize` size.
  ! -------------------------------------------------------------------------- !
  subroutine generatePopulation(popList, startPopSize, initMttnCount)
    type(PersonList), intent(inout) :: popList
      !! The list of `Person` objects to be initialized.
    integer,          intent(in)    :: startPopSize
      !! Starting population size.
    integer,          intent(in)    :: initMttnCount
      !! Initial mutation count.

    type(Person), pointer :: newIndiv_ptr
    type(Person), pointer :: oldIndiv_ptr
    integer :: i


    call initializeHealthyIndiv(popList % head_ptr, initMttnCount)
    oldIndiv_ptr => popList % head_ptr
    newIndiv_ptr => null()

    if (startPopSize == 1) then
      popList % tail_ptr => popList % head_ptr
    else
      do i = 1, startPopSize - 1
        allocate(newIndiv_ptr)
        oldIndiv_ptr % next => newIndiv_ptr
        oldIndiv_ptr => newIndiv_ptr
        
        call initializeHealthyIndiv(newIndiv_ptr, initMttnCount)
      end do
      
      popList % tail_ptr => newIndiv_ptr
    end if
  end subroutine generatePopulation

  
  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList % ]freePtr
  !>  Free the allocatable elements of the linked list.
  ! -------------------------------------------------------------------------- !
  subroutine freePtr(self, popSize)
    class(PersonList), intent(inout) :: self
      !! The linked-list of individuals represented as `Person` objects.
    integer,           intent(in)    :: popSize
      !! The size of `self`.

    type(Person), pointer :: curr_ptr => null()
    type(Person), pointer :: next_ptr => null()

    if (popSize == 0 .and. associated(self % head_ptr)) then
      self % head_ptr => null()
    end if

    curr_ptr => self % head_ptr
    next_ptr => null()
    do
      if (associated(curr_ptr)) then
        next_ptr => curr_ptr % next
        deallocate(curr_ptr)
        curr_ptr => next_ptr
      else
        exit
      end if
    end do
  end subroutine freePtr


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList % ]killCurrentIndiv
  !>  Kill the `Person` attribute the current pointer is pointing at
  !!  and remove it from the list.
  ! -------------------------------------------------------------------------- !
  subroutine killCurrentIndiv(self)
    class(PersonList), intent(inout) :: self
      !! The linked-list of individuals represented as `Person` objects.

    type(Person), pointer :: deadIndiv_ptr

    deadIndiv_ptr => self % current_ptr
    self % current_ptr => self % current_ptr % next
    if (associated(self % old_ptr)) self % old_ptr % next => self % current_ptr

    deallocate(deadIndiv_ptr)
  end subroutine killCurrentIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: incrementPtr
  !>  Have the `current_ptr` attribute of the passed `PersonList`
  !!  object to point at the next element of the linked list.
  ! -------------------------------------------------------------------------- !
  subroutine incrementPtr(list)
    class(PersonList), intent(inout) :: list
      !! The linked-list of individuals represented as `Person` objects.

    ! NOTE: We demand that `LL_nextElem` will never encounter a
    ! disassociated pointer.
    list % old_ptr => list % current_ptr
    list % current_ptr => list % current_ptr % next
  end subroutine incrementPtr


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList % ]reproduceCurrIndiv
  !>  Have the `Person` object the current pointer is pointing at to reproduce.
  ! -------------------------------------------------------------------------- !
  subroutine reproduceCurrIndiv(self, updateGenome)
    use Demographics, only: updateGenomeDstrb

    class(PersonList), intent(inout) :: self
      !! The linked-list of individuals represented as `Person` objects.
    logical,           intent(in)    :: updateGenome
      !! Update the genome distribution if true.

    type(Person), pointer :: newIndiv_ptr
    type(Person), pointer :: oldIndiv_ptr
    integer :: i

    if (MODEL_R > self % current_ptr % age &
        .or. self % current_ptr % age > MODEL_R_MAX) return

    ! Add new born indivs to the next generation.
    oldIndiv_ptr => self % newTail_ptr
    newIndiv_ptr => null()
    do i = 1, MODEL_B
      allocate(newIndiv_ptr)
      call initializeIndiv(newIndiv_ptr, self % current_ptr % genome)

      oldIndiv_ptr % next => newIndiv_ptr

      ! Update genome.
      if (updateGenome) call updateGenomeDstrb(newIndiv_ptr % genome)
    end do

    ! Update the new tail.
    self % newTail_ptr => newIndiv_ptr
  end subroutine reproduceCurrIndiv


  !-------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList % ]isCurrIndivDead
  !>  Check whether the `Person` object the current pointer is
  !!  pointing at is dead.
  ! -------------------------------------------------------------------------- !
  logical pure function isCurrIndivDead(self)
    class(PersonList), intent(in) :: self
      !! The linked-list of individuals represented as `Person` objects.

    isCurrIndivDead = self % current_ptr % deathIndex /= ALIVE
  end function isCurrIndivDead


  ! -------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList % ]getCurrIndivAge
  !>  Get the age of the `Person` object the current pointer is
  !!  pointing at.
  ! -------------------------------------------------------------------------- !
  integer function getCurrIndivAge(self)
    class(PersonList), intent(in) :: self
      !! The linked-list of individuals represented as `Person` objects.

    if (associated(self % current_ptr)) then
      getCurrIndivAge = self % current_ptr % age
    else
      print "(a)", "***ERROR. The current pointer of a linked list is " // &
          "disassociated."
      error stop
    end if
  end function getCurrIndivAge


  ! -------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList % ]getCurrIndivGenome
  !>  Get the genome of the `Person` object the current pointer is
  !!  pointing at.
  ! -------------------------------------------------------------------------- !
  function getCurrIndivGenome(self) result(genome)
    class(PersonList), intent(in) :: self
      !! The linked-list of individuals represented as `Person` objects.

    integer(kind=personIK) :: genome

    if (associated(self % current_ptr)) then
      genome = self % current_ptr % genome
    else
      print "(a)", "***ERROR. The current pointer of a linked list is " // &
          " disassociated."
      error stop
    end if
  end function getCurrIndivGenome


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList % ]nextElem
  !>  Proceed to the next element of the list. This also handles removal
  !!  of dead `Person` objects as removing the them will have the 
  !!  reader pointers points at the element next to the dead one
  !!  automatically.
  ! -------------------------------------------------------------------------- !
  subroutine nextElem(self, status)
    class(PersonList), intent(inout) :: self
      !! The linked-list of individuals represented as `Person` objects.
    integer,           intent(out)   :: status
      !! Status of this routine. Returns 0 if incrementing the `current_ptr` of
      !! `self` succeeds. Returns -1 if the end of the `self` is reached.

    ! ***Terminal case: end of the linked-list.
    !    This corresponds to `status` = -1.
    if (associated(self % current_ptr, self % tail_ptr)) then
      ! Check the tail's life.
      if (self % current_ptr % deathIndex /= ALIVE) then
        call self % killCurrentIndiv()

        ! Check edge case.
        if (associated(self % current_ptr)) then
          self % tail_ptr => self % current_ptr
        else
          self % current_ptr => self % old_ptr
          self % tail_ptr => self % current_ptr
          self % newTail_ptr => self % current_ptr

          ! NOTE: We will not bother decrementing his pointer since most likely
          ! `self % old_ptr` will be reset anyways.
          self % old_ptr => null()
        end if
      end if

      ! Update the tail of the linked list.
      status = -1
      self % tail_ptr => self % newTail_ptr

    ! ***Non-terminal case. Move to the next element of the list.
    ! This corresponds to `status` = 0.
    else
      ! Check life of the current individual.
      if (self % current_ptr % deathIndex == ALIVE) then
        call incrementPtr(self)
      else
        ! Check edge case.
        if (associated(self % current_ptr, self % head_ptr)) &
            self % head_ptr => self % current_ptr % next

        call self % killCurrentIndiv()
      end if
      
      status = 0
    end if
  end subroutine nextElem


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: [PersonList % ]resetReadPtrs
  !>  Reset reader pointers, i.e. `current_ptr` and `old_ptr` attributes,
  !!  of the linked list. The current pointer goes back to the head of
  !!  the list and the "previous" pointer is nullified.
  ! -------------------------------------------------------------------------- !
  subroutine resetReadPtrs(self)
    class(PersonList), intent(inout) :: self
      !! The linked-list of individuals represented as `Person` objects.
  
    self % current_ptr => self % head_ptr
    self % old_ptr => null()
  end subroutine resetReadPtrs


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList % ]updateCurrIndivAge
  !>  Update the age of the `Person` object `current_ptr` is pointing at.
  ! -------------------------------------------------------------------------- !
  subroutine updateCurrIndivAge(self)
    class(PersonList), intent(inout) :: self
      !! The linked-list of individuals represented as `Person` objects.

    self % current_ptr % age = self % current_ptr % age + 1
  end subroutine updateCurrIndivAge


  ! -------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList % ]isCurrIndivMature
  !>  Inquire whether the `Person` object the current pointer is pointing
  !!  at is able to reproduce.
  ! -------------------------------------------------------------------------- !
  logical pure function isCurrIndivMature(self)
    class(PersonList), intent(in) :: self
      !! The linked-list of individuals represented as `Person` objects.

    logical :: lowerBound
    logical :: upperBound

    lowerBound = MODEL_R <= self % current_ptr % age
    upperBound = self % current_ptr % age <= MODEL_R_MAX
  
    isCurrIndivMature = (lowerBound .and. upperBound)
  end function isCurrIndivMature


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList % ]checkCurrIndivDeath
  !>  Update the death status of the  `Person` object the current pointer is 
  !!  pointing at. 
  ! -------------------------------------------------------------------------- !
  subroutine checkCurrIndivDeath(self, popSize)
    use RNG, only: getRandNumber

    class(PersonList), intent(inout) :: self
      !! The linked-list of individuals represented as `Person` objects.
    integer,           intent(in)    :: popSize
      !! The current population size for calculating the Verhulst factor.
  
    real(kind=personRK) :: verhulstWeight
    real(kind=personRK) :: verhulstFactor
    integer :: nextAge
    real    :: random

    nextAge = self % current_ptr % age + 1          ! Hypothetical age
    verhulstWeight = MODEL_VERHULST_W(nextAge)  ! Verhulst weight per age

    ! ***Death check: Old age
    if (nextAge >= MODEL_L) then
      self % current_ptr % deathIndex = DEAD_OLD_AGE
      return
    end if

    ! Count mutation.
    if (getGene(self % current_ptr % genome, nextAge) == GENE_UNHEALTHY) &
      self % current_ptr % mutationCount = &
          self % current_ptr % mutationCount + 1

    ! ***Death check: Mutation accumulation
    if (self % current_ptr % mutationCount >= MODEL_T) then
      self % current_ptr % deathIndex = DEAD_MUTATION

    ! ***Death check: Verhulst factor
    else if (verhulstWeight > 0.0) then
      ! Get Verhulst factor per age.
      call getRandNumber(random)
      verhulstFactor = 1.0 - real(popSize)/real(MODEL_K)*verhulstWeight

      if (random > verhulstFactor) &
        self % current_ptr % deathIndex = DEAD_VERHULST
    end if
  end subroutine checkCurrIndivDeath
end module Pop

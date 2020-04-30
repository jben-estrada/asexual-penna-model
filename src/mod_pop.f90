module Pop
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Pop
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing evaluation and generation of population
  ! -------------------------------------------------------------------------- !
  use Gene
  use Parameters
  use ErrorMSG, only: raiseError
  implicit none
  private

  ! `PERSON` DERIVED TYPE
  ! -------------------------------------------------------------------------- !
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

  ! `PERSON` LINKED-LIST POINTERS.
  ! -------------------------------------------------------------------------- !
  ! List pointers.
  type(Person), pointer :: head_ptr => null()
      !! The head of the list.
  type(Person), pointer :: tail_ptr => null()
    !! The tail of the list at every beginning of each time steps. 
  type(Person), pointer :: newTail_ptr => null()
    !! The transient tail of the list during each time steps. At the end of 
    !! each time steps, `tail_ptr` is updated to point to this pointer.

  ! Reader pointers.
  type(Person), pointer :: current_ptr => null()
    !! The reading pointer for reading and modifying the list.
    !! This points to the current individual.
  type(Person), pointer :: old_ptr => null()
    !! The pointer before `current_ptr`.

  ! INDIVIDUAL STATES.
  ! -------------------------------------------------------------------------- !
  integer, public, parameter :: ALIVE = 1
  integer, public, parameter :: DEAD_OLD_AGE = 2
  integer, public, parameter :: DEAD_MUTATION = 3
  integer, public, parameter :: DEAD_VERHULST = 4
  integer, public, parameter :: DEATH_REASONS(*) = &
      [ALIVE,        &
      DEAD_OLD_AGE,  &
      DEAD_MUTATION, &
      DEAD_VERHULST]
  
  ! PUBLIC PROCEDURES
  ! -------------------------------------------------------------------------- !
  ! Inquiry procedures
  public :: isCurrIndivDead
  public :: isCurrIndivMature
  public :: getCurrIndivAge
  public :: getCurrIndivGenome
  public :: determineDeathType
  public :: elemCount

  ! Transformational procedures.
  public :: killCurrentIndiv
  public :: checkCurrIndivDeath
  public :: updateCurrIndivAge
  public :: reproduceCurrIndiv
  public :: nextElem

  ! Subroutines for memory management.
  public :: resetPersonReadPtrs
  public :: freePersonPtrs

  ! Initializer
  public :: initializePersonList
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializePersonList
  !>  Initialize the `Person` linked list with the provided initial conditions:
  !!  `startPopSize` for starting population size; and `initMttnCount` for
  !!  the initial mutation count.
  ! -------------------------------------------------------------------------- !
  subroutine initializePersonList(startPopsize, initMttnCount)
    integer, intent(in) :: startPopSize
      !! Starting population size.
    integer, intent(in) :: initMttnCount
      !! Initial mutation count.

    type(Person), pointer :: newIndiv_ptr
    type(Person), pointer :: oldIndiv_ptr
    integer :: i

    ! Initialize the head of the `Person` list.
    allocate(head_ptr)
    call initializeHealthyIndiv(head_ptr, initMttnCount)

    ! Append the new individuals.
    oldIndiv_ptr => head_ptr
    newIndiv_ptr => null()
    if (startPopSize == 1) then
      tail_ptr => head_ptr
    else
      do i = 1, startPopSize - 1
        allocate(newIndiv_ptr)
        oldIndiv_ptr % next => newIndiv_ptr
        oldIndiv_ptr => newIndiv_ptr
        
        call initializeHealthyIndiv(newIndiv_ptr, initMttnCount)
      end do
      
      tail_ptr => newIndiv_ptr
    end if

    newTail_ptr => tail_ptr

    ! Initialize the reader pointer.
    current_ptr => head_ptr
    old_ptr => null()
  end subroutine initializePersonList


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeHealthyIndiv
  !>  Initialize the `Person` object `indiv_ptr` is pointing to `Person` with 
  !!  a healthy genome by default.
  ! -------------------------------------------------------------------------- !
  subroutine initializeHealthyIndiv(indiv_ptr, mutationCount)
    type(Person), pointer, intent(inout) :: indiv_ptr
      !! The pointer to the individual or the `Person` object to be initialized.
    integer,               intent(in)    :: mutationCount
      !! Initial number of mutations.

    ! Initialize genome and mutation count.
    indiv_ptr % genome = GENE_HEALTHY
    indiv_ptr % mutationCount = 0
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

    integer :: mutationIndcs(mutationCount)
    integer :: i

    if (mutationCount > 0) then
      ! Get random indices of genes to mutate.
      call generateIndices(1, MODEL_L, mutationIndcs)

      ! Apply mutations.
      do i = 1, mutationCount
        indiv_ptr % genome = ior(indiv_ptr % genome, &
            int(shiftl(1, mutationIndcs(i) - 1), kind=personIK))
      end do
    end if
  end subroutine applyInitialMutations


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: determineDeathType
  !>  Determine the death of the current individual. This routine fails if
  !!  the current individual is alive.
  ! -------------------------------------------------------------------------- !
  subroutine determineDeathType(deathByAge, deathByMutation, deathByVerhulst)
    integer, pointer, intent(inout) :: deathByAge
      !! Pointer for death by age counter.
    integer, pointer, intent(inout) :: deathByMutation
      !! Pointer for death by mutation.
    integer, pointer, intent(inout) :: deathByVerhulst
      !! Pointer for death by Verhulst killing.

    select case (current_ptr % deathIndex)
      case (DEAD_OLD_AGE)
        deathByAge = deathByAge + 1

      case (DEAD_MUTATION)
        deathByMutation = deathByMutation + 1

      case (DEAD_VERHULST)
        deathByVerhulst = deathByVerhulst + 1

      case default
        call raiseError("The current individual is not dead or has an" // &
            " invalid death index.")
    end select
  end subroutine determineDeathType


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
    call generateIndices(1, MODEL_L, mutations)

    indiv_ptr % genome = genome
    do i = 1, size(mutations)
      if (getGene(indiv_ptr % genome, mutations(i)) == GENE_HEALTHY) then
        indiv_ptr % genome = ior(indiv_ptr % genome, &
            shiftl(1_personIK, mutations(i) - 1))
      end if
    end do

    indiv_ptr % age = 0
    indiv_ptr % mutationCount = 0
    indiv_ptr % deathIndex = ALIVE
  end subroutine initializeIndiv

  
  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freePersonPtrs
  !>  Free the allocatable elements of the `Person` linked list.
  ! -------------------------------------------------------------------------- !
  subroutine freePersonPtrs(popSize)
    integer, intent(in) :: popSize
      !! The size of `self`.

    ! Local reader pointers.
    type(Person), pointer :: localCurr_ptr => null()
    type(Person), pointer :: localNext_ptr => null()

    if (popSize == 0 .and. associated(head_ptr)) then
      head_ptr => null()
    end if

    localCurr_ptr => head_ptr
    localNext_ptr => null()
    do
      if (associated(localCurr_ptr)) then
        localNext_ptr => localCurr_ptr % next
        deallocate(localCurr_ptr)
        localCurr_ptr => localNext_ptr
      else
        exit
      end if
    end do
  end subroutine freePersonPtrs


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: killCurrentIndiv
  !>  Kill the `Person` attribute `current_ptr` is pointing at  and remove it 
  !!  from the list.
  ! -------------------------------------------------------------------------- !
  subroutine killCurrentIndiv()
    type(Person), pointer :: deadIndiv_ptr ! Pointer for `Person` to be killed.

    deadIndiv_ptr => current_ptr
    current_ptr => current_ptr % next
    if (associated(old_ptr)) old_ptr % next => current_ptr

    deallocate(deadIndiv_ptr)
  end subroutine killCurrentIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: incrementPtr
  !>  Have the `dummyCurr_ptr` point to the next element of the linked list.
  ! -------------------------------------------------------------------------- !
  subroutine incrementPtr(dummyCurr_ptr, dummyOld_ptr)
    type(Person), pointer, intent(inout) :: dummyCurr_ptr
      !! Dummy argument for the current reader pointer.
    type(Person), pointer, intent(inout) :: dummyOld_ptr
    !! Dummy argument for the previous reader pointer.

    ! NOTE: We demand that `dummyCurr_ptr` will never encounter a
    ! disassociated pointer.
    dummyOld_ptr => dummyCurr_ptr
    dummyCurr_ptr => dummyCurr_ptr % next
  end subroutine incrementPtr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: reproduceCurrIndiv
  !>  Have the `Person` object `current_ptr` is pointing at to reproduce.
  ! -------------------------------------------------------------------------- !
  subroutine reproduceCurrIndiv(updateGenome)
    use Demographics, only: updateGenomeDstrb

    logical, intent(in) :: updateGenome
      !! Update the genome distribution if true.

    type(Person), pointer :: newIndiv_ptr
    type(Person), pointer :: oldIndiv_ptr
    integer :: i

    ! Check if the current individual can reproduce.
    if (MODEL_R > current_ptr % age .or. current_ptr % age > MODEL_R_MAX) return

    ! Add new born individuals to the next generation.
    oldIndiv_ptr => newTail_ptr
    newIndiv_ptr => null()
    do i = 1, MODEL_B
      allocate(newIndiv_ptr)

      ! Pass the genome of the current individual to the newborn ones.
      call initializeIndiv(newIndiv_ptr, current_ptr % genome)

      ! Proceed to the next element on the list.
      oldIndiv_ptr % next => newIndiv_ptr
      oldIndiv_ptr => newIndiv_ptr

      ! Incremenet the genome counter or add a new "genome node" instance to
      ! be appended onto the genome list.
      if (updateGenome) call updateGenomeDstrb(newIndiv_ptr % genome)
    end do

    ! Update the new tail.
    newTail_ptr => newIndiv_ptr
  end subroutine reproduceCurrIndiv


  !-------------------------------------------------------------------------- !
  ! FUNCTION: isCurrIndivDead
  !>  Check whether the `Person` object `current_ptr` is pointing at is dead.
  ! -------------------------------------------------------------------------- !
  logical pure function isCurrIndivDead()
    isCurrIndivDead = (current_ptr % deathIndex /= ALIVE)
  end function isCurrIndivDead


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCurrIndivAge
  !>  Get the age of the `Person` object `current_ptr` is pointing at.
  ! -------------------------------------------------------------------------- !
  integer function getCurrIndivAge()
    ! Initialize output with 0.
    getCurrIndivAge = 0

    if (associated(current_ptr)) then
      getCurrIndivAge = current_ptr % age
    else
      call raiseError("The current pointer of a linked list is disassociated.")
    end if
  end function getCurrIndivAge


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCurrIndivGenome
  !>  Get the genome of the `Person` object `current_ptr` is pointing at.
  ! -------------------------------------------------------------------------- !
  function getCurrIndivGenome() result(genome)
    integer(kind=personIK) :: genome

    ! Initialize the output with 0.
    genome = 0_personIK

    if (associated(current_ptr)) then
      genome = current_ptr % genome
    else
      call raiseError("The current pointer of a linked list is disassociated.")
    end if
  end function getCurrIndivGenome


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: nextElem
  !>  Proceed to the next element of the list.
  ! -------------------------------------------------------------------------- !
  subroutine nextElem(status)
    integer, intent(out) :: status
      !! Status of this routine. Returns 0 if incrementing the `current_ptr` of
      !! `self` succeeds. Returns -1 if the end of the `self` is reached.

    ! ***Terminal case: end of the linked-list.
    !    This corresponds to `status` = -1.
    if (associated(current_ptr, tail_ptr)) then
      ! Handle the case when the tail `Person` is dead.
      if (current_ptr % deathIndex /= ALIVE) then
        call killCurrentIndiv()

        ! Check edge case.
        if (associated(current_ptr)) then
          tail_ptr => current_ptr
        else
          ! Have `current_ptr` point back to the previous ptr.
          current_ptr => old_ptr
          tail_ptr => current_ptr
          newTail_ptr => current_ptr

          ! NOTE: We will not bother decrementing `old_ptr` since most likely
          ! it will be reset anyways.
          old_ptr => null()
        end if
      end if

      ! Update the tail of the linked list.
      status = -1
      tail_ptr => newTail_ptr

    ! ***Non-terminal case. Move to the next element of the list.
    ! This corresponds to `status` = 0.
    else
      ! Check the life of the current individual.
      if (current_ptr % deathIndex == ALIVE) then
        call incrementPtr(current_ptr, old_ptr)
      else
        ! Handle the case when the head `Person` is dead.
        if (associated(current_ptr, head_ptr)) &
            head_ptr => current_ptr % next

        call killCurrentIndiv()
      end if
      
      status = 0
    end if
  end subroutine nextElem


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: resetPersonReadPtrs
  !>  Reset reader pointers, i.e. `current_ptr` and `old_ptr`. 
  !!  The current pointer goes back to the head of the list and the "previous" 
  !!  pointer is nullified.
  ! -------------------------------------------------------------------------- !
  subroutine resetPersonReadPtrs()
    current_ptr => head_ptr
    old_ptr => null()
  end subroutine resetPersonReadPtrs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: updateCurrIndivAge
  !>  Increment the age of the `Person` object `current_ptr` is pointing at.
  ! -------------------------------------------------------------------------- !
  subroutine updateCurrIndivAge()
    current_ptr % age = current_ptr % age + 1
  end subroutine updateCurrIndivAge


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isCurrIndivMature
  !>  Inquire whether the `Person` object the `current_ptr` is pointing at is
  !!  able to reproduce.
  ! -------------------------------------------------------------------------- !
  logical pure function isCurrIndivMature()
    logical :: lowerBound
    logical :: upperBound

    lowerBound = MODEL_R <= current_ptr % age
    upperBound = current_ptr % age <= MODEL_R_MAX
  
    isCurrIndivMature = (lowerBound .and. upperBound)
  end function isCurrIndivMature


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkCurrIndivDeath
  !>  Update the death status of the `Person` object the `current_ptr` is 
  !!  pointing at. 
  ! -------------------------------------------------------------------------- !
  subroutine checkCurrIndivDeath(popSize)
    use RNG, only: getRandNumber

    integer,           intent(in)    :: popSize
      !! The current population size for calculating the Verhulst factor.
  
    real(kind=personRK) :: verhulstWeight
    real(kind=personRK) :: verhulstFactor
    integer :: nextAge
    real    :: random

    nextAge = current_ptr % age + 1             ! Hypothetical age
    verhulstWeight = MODEL_V_WEIGHT(nextAge)  ! Verhulst weight per age

    ! ***Death check: Old age
    if (nextAge >= MODEL_L) then
      current_ptr % deathIndex = DEAD_OLD_AGE
      return
    end if

    ! Count mutation.
    if (getGene(current_ptr % genome, nextAge) == GENE_UNHEALTHY) &
      current_ptr % mutationCount = current_ptr % mutationCount + 1

    ! ***Death check: Mutation accumulation
    if (current_ptr % mutationCount >= MODEL_T) then
      current_ptr % deathIndex = DEAD_MUTATION

    ! ***Death check: Verhulst factor
    else if (verhulstWeight > 0.0_personIK) then
      ! Get Verhulst factor per age.
      call getRandNumber(random)
      verhulstFactor = 1.0 - real(popSize)/real(MODEL_K)*verhulstWeight

      if (random > verhulstFactor) &
        current_ptr % deathIndex = DEAD_VERHULST
    end if
  end subroutine checkCurrIndivDeath


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: elemCount
  !>  Count the number of elements of the linked-list. This is usually used
  !!  for debugging.
  ! -------------------------------------------------------------------------- !
  integer function elemCount()
    type(Person), pointer :: localCurr_ptr

    ! Initialize local variables.
    localCurr_ptr => head_ptr
    elemCount = 0
    do
      ! Count the associated pointers.
      if (associated(localCurr_ptr)) then
        elemCount = elemCount + 1
        ! Get to the next element of the list.
        localCurr_ptr => localCurr_ptr % next

      ! Exit out of the loop if the pointer is out of bounds.
      else
        exit
      end if
    end do
  end function elemCount
end module Pop

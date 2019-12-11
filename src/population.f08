module Pop
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Pop
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing evaluation and generation of population
  ! -------------------------------------------------------------------------- !
  use Gene
  use Flag
  use ModelParam
  implicit none
  private

  ! `Person` derived type. A node to the population linked-list.
  type :: Person
    integer(kind=personIK) :: genome
    integer :: age
    integer :: deathIndex
    integer :: mutationCount

    type(Person), pointer  :: next => null() ! Next node in linked list
  end type Person


  ! Linked-list derived type.
  type, public :: PersonList
    private
    type(Person), pointer :: head_ptr => null()
    type(Person), pointer :: tail_ptr => null()
    type(Person), pointer :: newTail_ptr => null()

    type(Person), pointer :: old_ptr => null()
    type(Person), pointer :: current_ptr => null()
  contains
    ! Inquiry functions.
    procedure :: isCurrIndivDead
    procedure :: isCurrIndivMature
    procedure :: getCurrIndivDeathIdx
    procedure :: getCurrIndivAge
    procedure :: getCurrIndivGenome

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

  public :: constructPersonList
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: constructPersonList
  !>  Initialize a `PersonList` object.
  ! -------------------------------------------------------------------------- !
  function constructPersonList(startPopSize) result(newLL)
    implicit none
    integer, intent(in) :: startPopSize

    type(PersonList)    :: newLL
    
    allocate(newLL%head_ptr)
    call generatePopulation(newLL, startPopSize)
    newLL%newTail_ptr => newLL%tail_ptr
    newLL%current_ptr => newLL%head_ptr
  end function constructPersonList


  !----------------------------------------------------------------------------!
  ! FUNCTION: getBinDigit
  !>  Get the `k`th binary digit of the integer `number`.
  !----------------------------------------------------------------------------!
  function getBinDigit(number, k) result(bit)
    implicit none

    integer(kind=personIK), intent(in) :: number
    integer,                intent(in) :: k
    integer(kind=personIK) :: bit

    bit = 0
    bit = iand(shiftr(number, k - 1), 1_personIK)
  end function getBinDigit


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeHealthyIndiv
  !>  Initialize `indiv` with a healthy genome.
  ! -------------------------------------------------------------------------- !
  subroutine initializeHealthyIndiv(indiv_ptr)
    implicit none
    type(Person), pointer, intent(inout) :: indiv_ptr

    indiv_ptr%genome = GENE_HEALTHY
    indiv_ptr%age = 0_personIK
    indiv_ptr%mutationCount = 0
    indiv_ptr%deathIndex = ALIVE
  end subroutine initializeHealthyIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeIndiv
  !>  Initialize `indiv` with genes inherited from another `Person`
  !!  object.
  ! -------------------------------------------------------------------------- !
  subroutine initializeIndiv(indiv_ptr, genome)
    use RandInd, only: generateIndices
    implicit none

    type(Person), pointer,  intent(inout) :: indiv_ptr
    integer(kind=personIK), intent(in)    :: genome

    integer :: mutations(MODEL_M)
    integer :: i

    mutations(:) = 0  ! Initialize `mutations`
    call generateIndices(1, MODEL_L, mutations)

    indiv_ptr%genome = genome
    do i = 1, size(mutations)
      if (getBinDigit(indiv_ptr%genome, mutations(i)) == GENE_HEALTHY) then
        indiv_ptr%genome = ior(indiv_ptr%genome, &
            int(shiftl(1, mutations(i) - 1), kind=personIK))
      end if
    end do

    indiv_ptr%age = 0_personIK
    indiv_ptr%mutationCount = 0
    indiv_ptr%deathIndex = ALIVE
  end subroutine initializeIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: generatePopulation
  !>  Generate a list of population of `startPopSize` size.
  ! -------------------------------------------------------------------------- !
  subroutine generatePopulation(popList, startPopSize)
    implicit none

    type(PersonList), intent(inout) :: popList
    integer,          intent(in)    :: startPopSize

    type(Person), pointer :: newIndiv_ptr
    type(Person), pointer :: oldIndiv_ptr
    integer :: i

    call initializeHealthyIndiv(popList%head_ptr)
    oldIndiv_ptr => popList%head_ptr
    newIndiv_ptr => null()

    if (startPopSize == 1) then
      popList%tail_ptr => popList%head_ptr
    else
      do i = 1, startPopSize - 1
        allocate(newIndiv_ptr)
        oldIndiv_ptr%next => newIndiv_ptr
        oldIndiv_ptr => newIndiv_ptr
        
        call initializeHealthyIndiv(newIndiv_ptr)
      end do
      
      popList%tail_ptr => newIndiv_ptr
    end if
  end subroutine generatePopulation

  
  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList%]freePtr
  !>  Free the allocatable elements of the linked list.
  ! -------------------------------------------------------------------------- !
  subroutine freePtr(self, popSize)
    implicit none

    class(PersonList), intent(inout) :: self
    integer,           intent(in)    :: popSize

    type(Person), pointer :: curr_ptr => null()
    type(Person), pointer :: next_ptr => null()

    if (popSize == 0 .and. associated(self%head_ptr)) then
      self%head_ptr => null()
    end if

    curr_ptr => self%head_ptr
    next_ptr => null()
    do
      if (associated(curr_ptr)) then
        next_ptr => curr_ptr%next
        deallocate(curr_ptr)
        curr_ptr => next_ptr
      else
        exit
      end if
    end do
  end subroutine freePtr


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList%]killCurrentIndiv
  !>  Kill the `Person` attribute the current pointer is pointing at
  !!  and remove it from the list.
  ! -------------------------------------------------------------------------- !
  subroutine killCurrentIndiv(self)
    implicit none
    class(PersonList), intent(inout) :: self

    type(Person), pointer :: deadIndiv_ptr

    deadIndiv_ptr => self%current_ptr
    self%current_ptr => self%current_ptr%next
    if (associated(self%old_ptr)) self%old_ptr%next => self%current_ptr

    deallocate(deadIndiv_ptr)
  end subroutine killCurrentIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: incrementPtr
  !>  Have the `current_ptr` attribute of the passed `PersonList`
  !!  object to point at the next element of the linked list.
  ! -------------------------------------------------------------------------- !
  subroutine incrementPtr(list)
    implicit none
    class(PersonList), intent(inout) :: list

    ! NOTE: We demand that `LL_nextElem` will never encounter a
    ! disassociated pointer.
    list%old_ptr => list%current_ptr
    list%current_ptr => list%current_ptr%next
  end subroutine incrementPtr


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList%]reproduceCurrIndiv
  !>  Have the `Person` object the current pointer is pointing at
  !!  to reproduce.
  ! -------------------------------------------------------------------------- !
  subroutine reproduceCurrIndiv(self)
    implicit none
    class(PersonList), intent(inout) :: self

    type(Person), pointer :: newIndiv_ptr
    type(Person), pointer :: oldIndiv_ptr
    integer :: i

    if (MODEL_R > self%current_ptr%age &
        .or. self%current_ptr%age > MODEL_R_MAX) return

    ! Add new born indivs to the next generation.
    oldIndiv_ptr => self%newTail_ptr
    newIndiv_ptr => null()
    do i = 1, MODEL_B
      allocate(newIndiv_ptr)
      call initializeIndiv(newIndiv_ptr, self%current_ptr%genome)

      oldIndiv_ptr%next => newIndiv_ptr
    end do

    ! Update the new tail.
    self%newTail_ptr => newIndiv_ptr
  end subroutine reproduceCurrIndiv


  !-------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList%]isCurrIndivDead
  !>  Check whether the `Person` object the current pointer is
  !!  pointing at is dead.
  ! -------------------------------------------------------------------------- !
  logical function isCurrIndivDead(self)
    implicit none
    class(PersonList), intent(inout) :: self

    isCurrIndivDead = self%current_ptr%deathIndex /= ALIVE
  end function isCurrIndivDead


  ! -------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList%]getCurrIndivDeathIdx
  !>  Get the death index of the `Person` object the current pointer is
  !!  pointing at.
  ! -------------------------------------------------------------------------- !
  integer function getCurrIndivDeathIdx(self)
    implicit none
    class(PersonList), intent(in) :: self

    if (associated(self%current_ptr)) then
      getCurrIndivDeathIdx = self%current_ptr%deathIndex
    else
      getCurrIndivDeathIdx = -1  ! An invalid value.
    end if
  end function getCurrIndivDeathIdx


  ! -------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList%]getCurrIndivAge
  !>  Get the age of the `Person` object the current pointer is
  !!  pointing at.
  ! -------------------------------------------------------------------------- !
  integer function getCurrIndivAge(self)
    implicit none
    class(PersonList), intent(in) :: self

    if (associated(self%current_ptr)) then
      getCurrIndivAge = self%current_ptr%age
    else
      error stop "The current pointer of a linked list is disassociated."
    end if
  end function getCurrIndivAge


  ! -------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList%]getCurrIndivGenome
  !>  Get the genome of the `Person` object the current pointer is
  !!  pointing at.
  ! -------------------------------------------------------------------------- !
  function getCurrIndivGenome(self) result(genome)
    implicit none
    class(PersonList), intent(in) :: self

    integer(kind=personIK) :: genome

    if (associated(self%current_ptr)) then
      genome = self%current_ptr%genome
    else
      error stop "The current pointer of a linked list is disassociated."
    end if
  end function getCurrIndivGenome


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList%]nextElem
  !>  Proceed to the next element of the list. This also handles removal
  !!  of dead `Person` objects as removing the them will have the 
  !!  reader pointers points at the element next to the dead one
  !!  automatically.
  ! -------------------------------------------------------------------------- !
  subroutine nextElem(self, status)
    implicit none

    class(PersonList), intent(inout) :: self
    integer,           intent(out)   :: status

    ! ***Terminal case: end of the linked-list.
    !    This corresponds to `status` = -1.
    if (associated(self%current_ptr, self%tail_ptr)) then
      ! Check the tail's life.
      if (self%current_ptr%deathIndex /= ALIVE) then
        call self%killCurrentIndiv()

        ! Check edge case.
        if (associated(self%current_ptr)) then
          self%tail_ptr => self%current_ptr
        else
          self%current_ptr => self%old_ptr
          self%tail_ptr => self%current_ptr
          self%newTail_ptr => self%current_ptr

          ! NOTE: We will not bother decrementing his pointer since most likely
          ! `self%old_ptr` will be reset anyways.
          self%old_ptr => null()
        end if
      end if

      ! Update the tail of the linked list.
      status = -1
      self%tail_ptr => self%newTail_ptr

    ! ***Non-terminal case. Move to the next element of the list.
    ! This corresponds to `status` = 0.
    else
      ! Check life of the current individual.
      if (self%current_ptr%deathIndex == ALIVE) then
        call incrementPtr(self)
      else
        ! Check edge case.
        if (associated(self%current_ptr, self%head_ptr)) &
            self%head_ptr => self%current_ptr%next

        call self%killCurrentIndiv()
      end if
      
      status = 0
    end if
  end subroutine nextElem


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: [PersonList%]resetReadPtrs
  !>  Reset reader pointers, i.e. `current_ptr` and `old_ptr` attributes,
  !!  of the linked list. The current pointer goes back to the head of
  !!  the list and the "previous" pointer is nullified.
  ! -------------------------------------------------------------------------- !
  subroutine resetReadPtrs(self)
    implicit none
    class(PersonList), intent(inout) :: self
  
    self%current_ptr => self%head_ptr
    self%old_ptr => null()
  end subroutine resetReadPtrs


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList%]updateCurrIndivAge
  !>  Update the age of the `Person` object `current_ptr` is pointing at.
  ! -------------------------------------------------------------------------- !
  subroutine updateCurrIndivAge(self)
    implicit none
    class(PersonList), intent(inout) :: self
  
    self%current_ptr%age = self%current_ptr%age + 1
  end subroutine updateCurrIndivAge


  ! -------------------------------------------------------------------------- !
  ! BOUND FUNCTION: [PersonList%]isCurrIndivMature
  !>  Inquire whether the `Person` object the current pointer is pointing
  !!  at is able to reproduce.
  ! -------------------------------------------------------------------------- !
  logical function isCurrIndivMature(self)
    implicit none
    class(PersonList), intent(inout) :: self

    logical :: lowerBound
    logical :: upperBound

    lowerBound = MODEL_R <= self%current_ptr%age
    upperBound = self%current_ptr%age <= MODEL_R_MAX
  
    isCurrIndivMature = (lowerBound .and. upperBound)
  end function isCurrIndivMature


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [PersonList%]checkCurrIndivDeath
  !>  Update the death status of the  `Person` object the current
  !!  pointer is pointing at. 
  ! -------------------------------------------------------------------------- !
  subroutine checkCurrIndivDeath(self, popSize)
    implicit none

    class(PersonList), intent(inout) :: self
    integer,           intent(in)    :: popSize
  
    real(kind=personRK) :: verhulstWeight
    real(kind=personRK) :: verhulstFactor
    real(kind=personRK) :: random
    integer :: nextAge

    nextAge = self%current_ptr%age + 1          ! Hypothetical age
    verhulstWeight = MODEL_VERHULST_W(nextAge)  ! Verhulst weight per age

    ! ***Death check: Old age
    if (nextAge >= MODEL_L) then
      self%current_ptr%deathIndex = DEAD_OLD_AGE
      return
    end if

    ! Count mutation.
    if (getBinDigit(self%current_ptr%genome, nextAge) == GENE_UNHEALTHY) &
      self%current_ptr%mutationCount = self%current_ptr%mutationCount + 1

    ! ***Death check: Mutation accumulation
    if (self%current_ptr%mutationCount >= MODEL_T) then
      self%current_ptr%deathIndex = DEAD_MUTATION

    ! ***Death check: Verhulst factor
    else if (verhulstWeight > 0.0) then
      ! Get Verhulst factor per age.
      call random_number(random)
      verhulstFactor = 1.0 - real(popSize)/real(MODEL_K)*verhulstWeight

      if (random > verhulstFactor) self%current_ptr%deathIndex = DEAD_VERHULST
    end if
  end subroutine checkCurrIndivDeath
end module Pop

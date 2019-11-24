module PersonType
  ! -------------------------------------------------------------------------- !
  ! MODULE:  PersonType
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing the `Person` derived type.
  ! -------------------------------------------------------------------------- !
  use Model, only: MODEL_L
  use StdKind, only: personIntKind, personRealKind
  implicit none
  private

  type, public :: Person
    integer(kind=personIntKind) :: age
    integer(kind=personIntKind) :: genome
    integer                     :: deathIndex     ! Only counters
    integer                     :: mutationCount  ! Only counters

    type(Person), pointer       :: next => null() ! Next node in linked list
  end type Person
end module PersonType


module Pop
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Pop
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing evaluation and generation of population
  ! -------------------------------------------------------------------------- !
  implicit none
  private

  public :: checkDeath
  public :: checkBirth
  public :: killIndiv
  public :: generatePopulation
  public :: initializeHealthyIndiv
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkDeath
  !>  Check whether `indiv` will die in the current time step or not.
  ! -------------------------------------------------------------------------- !
  subroutine checkDeath(indiv_ptr, popSize, indexOffset)
    use Model
    use Flag
    use PersonType
    use Gene, only: GENE_UNHEALTHY
    use StdKind, only: personIntKind, personRealKind
    implicit none

    type(Person), pointer, intent(inout) :: indiv_ptr
    integer,               intent(inout) :: indexOffset
    integer,               intent(inout) :: popSize

    integer(kind=personIntKind) :: nextAge = 0
    real(kind=personRealKind)   :: verhulstWeight
    real(kind=personRealKind)   :: verhulstFactor
    real(kind=personRealKind)   :: random

    nextAge = indiv_ptr%age + 1                 ! Hypothetical age
    verhulstWeight = MODEL_VERHULST_W(nextAge)  ! Verhulst weight per age

    ! ***Death check: Old age
    if (nextAge >= MODEL_L) then
      indiv_ptr%deathIndex = DEAD_OLD_AGE
      indexOffset = indexOffset - 1
      return
    end if

    if (getBinDigit(indiv_ptr%genome, nextAge) == GENE_UNHEALTHY) then
      indiv_ptr%mutationCount = indiv_ptr%mutationCount + 1
    end if

    ! ***Death check: Mutation accumulation
    if (indiv_ptr%mutationCount >= MODEL_T) then
      indiv_ptr%deathIndex = DEAD_MUTATION
      indexOffset = indexOffset - 1
      return
    ! ***Death check: Verhulst factor
    else if (verhulstWeight > 0.0) then
      ! Get Verhulst factor per age.
      call random_number(random)
      verhulstFactor = 1.0 - real(popSize)/real(MODEL_K)*verhulstWeight

      if (random > verhulstFactor) then
        indiv_ptr%deathIndex = DEAD_VERHULST
        indexOffset = indexOffset - 1
        return
      end if
    end if
  end subroutine checkDeath

  !----------------------------------------------------------------------------!
  ! SUBROUTINE: killIndiv
  !>  Remove object `indiv_ptr` is pointing at from the population list.
  !----------------------------------------------------------------------------!
  subroutine killIndiv(currIndiv_ptr, oldIndiv_ptr)
    use PersonType
    implicit none

    type(Person), pointer, intent(inout) :: currIndiv_ptr
    type(Person), pointer, intent(inout) :: oldIndiv_ptr
    type(Person), pointer                :: deadIndiv_ptr

    deadIndiv_ptr => currIndiv_ptr
    currIndiv_ptr => currIndiv_ptr%next
    if (associated(oldIndiv_ptr)) oldIndiv_ptr%next => currIndiv_ptr

    deallocate(deadIndiv_ptr)
  end subroutine killIndiv


  !----------------------------------------------------------------------------!
  ! FUNCTION: getBinDigit
  !>  Get the `k`th binary digit of the integer `number`.
  !----------------------------------------------------------------------------!
  function getBinDigit(number, k) result(bit)
    use StdKind, only: personIntKind, personRealKind
    implicit none

    integer(kind=personIntKind), intent(in) :: number
    integer(kind=personIntKind), intent(in) :: k
    integer(kind=personIntKind)             :: bit

    bit = 0
    bit = iand(shiftr(number, k - 1), 1_personIntKind)
  end function getBinDigit


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkBirth
  !>  Check whether `indiv` will reproduce at the current time step.
  ! -------------------------------------------------------------------------- !
  subroutine checkBirth(indiv_ptr, popFutureTail_ptr, indexOffset)
    use Model
    use PersonType
    implicit none

    type(Person), pointer, intent(in)    :: indiv_ptr
    type(Person), pointer, intent(inout) :: popFutureTail_ptr
    integer,               intent(inout) :: indexOffset

    type(Person), pointer :: newIndiv_ptr
    type(Person), pointer :: oldIndiv_ptr
    integer               :: i

    ! Check for valid reproduction age.
    if (MODEL_R > indiv_ptr%age .or. indiv_ptr%age > MODEL_R_MAX) return

    ! Add new born indivs to the next generation.
    oldIndiv_ptr => popFutureTail_ptr
    newIndiv_ptr => null()
    do i = 1, MODEL_B
      allocate(newIndiv_ptr)
      call initializeIndiv(newIndiv_ptr, indiv_ptr%genome)

      oldIndiv_ptr%next => newIndiv_ptr
      indexOffset = indexOffset + 1
    end do

    ! Update future tail of linked-list.
    popFutureTail_ptr => newIndiv_ptr
  end subroutine checkBirth

  
  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeHealthyIndiv
  !>  Initialize `indiv` with a healthy genome.
  ! -------------------------------------------------------------------------- !
  subroutine initializeHealthyIndiv(indiv_ptr)
    use PersonType
    use Flag, only: ALIVE
    use Gene, only: GENE_HEALTHY
    use StdKind, only: personIntKind
    implicit none

    type(Person), pointer, intent(inout) :: indiv_ptr
    indiv_ptr%genome = GENE_HEALTHY
    indiv_ptr%age = 0_personIntKind
    indiv_ptr%mutationCount = 0
    indiv_ptr%deathIndex = ALIVE
  end subroutine initializeHealthyIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeIndiv
  !>  Initialize `indiv` with genes inherited from another `Person`
  !!  type.
  ! -------------------------------------------------------------------------- !
  subroutine initializeIndiv(indiv_ptr, genome)
    use PersonType
    use Flag, only: ALIVE
    use Gene, only: GENE_HEALTHY
    use Model, only: MODEL_M, MODEL_L
    use RandInd, only: generateIndices
    use StdKind, only: personIntKind, personRealKind
    implicit none

    type(Person), pointer,       intent(inout) :: indiv_ptr
    integer(kind=personIntKind), intent(in)    :: genome
    integer(kind=personIntKind)                :: mutations(MODEL_M)
    integer :: i    ! Counters have default kind.

    mutations(:) = 0  ! Initialize `mutations`
    call generateIndices(1_personIntKind, int(MODEL_L, kind=personIntKind), &
        mutations)

    indiv_ptr%genome = genome
    do i = 1, size(mutations)
      if (getBinDigit(indiv_ptr%genome, mutations(i)) == GENE_HEALTHY) then
        indiv_ptr%genome = ior(indiv_ptr%genome, &
            int(shiftl(1, mutations(i) - 1), kind=personIntKind))
      end if
    end do

    indiv_ptr%age = 0_personIntKind
    indiv_ptr%mutationCount = 0
    indiv_ptr%deathIndex = ALIVE
  end subroutine initializeIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: generatePopulation
  !>  Generate population of `startPopSize` size.
  ! -------------------------------------------------------------------------- !
  subroutine generatePopulation(popHead_ptr, popTail_ptr, startPopSize)
    use PersonType
    implicit none
    type(Person), pointer, intent(inout) :: popHead_ptr
    type(Person), pointer, intent(out)   :: popTail_ptr
    integer,               intent(in)    :: startPopSize

    type(Person), pointer :: newIndiv_ptr
    type(Person), pointer :: oldIndiv_ptr
    integer               :: i

    oldIndiv_ptr => popHead_ptr
    newIndiv_ptr => null()
    call initializeHealthyIndiv(popHead_ptr)

    if (startPopSize == 1) then
      popTail_ptr => popHead_ptr
    else
      do i = 1, startPopSize - 1
        allocate(newIndiv_ptr)
        oldIndiv_ptr%next => newIndiv_ptr
        oldIndiv_ptr => newIndiv_ptr
        
        call initializeHealthyIndiv(newIndiv_ptr)
      end do
      
      popTail_ptr => newIndiv_ptr
    end if
  end subroutine generatePopulation
end module Pop

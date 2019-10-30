module PersonType
  ! -------------------------------------------------------------------------- !
  ! MODULE:  PersonType
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing the `Person` derived type.
  ! -------------------------------------------------------------------------- !
  use Model, only: MODEL_L, stdIntKind, stdRealKind
  implicit none

  type :: Person
    integer(kind=stdIntKind) :: age
    integer(kind=stdIntKind) :: genome
    integer :: deathIndex               ! Only counters
    integer :: mutationCount            ! Only counters
  end type Person
end module PersonType


module Pop
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Pop
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing evaluation and generation of population
  ! -------------------------------------------------------------------------- !
  use Model
  use Gene
  use Flag
  use PersonType
  use RandInd, only: generateIndices
  implicit none
  private

  public :: checkDeath
  public :: checkBirth
  public :: generatePopulation
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkDeath
  !>  Check whether `indiv` will die in the current time step or not.
  ! -------------------------------------------------------------------------- !
  subroutine checkDeath(indiv, popSize, indexOffset)
    implicit none
    integer, intent(inout) :: indexOffset   ! Counters have default kind.
    integer, intent(inout) :: popSize       ! Counters have default kind.
    type(Person), intent(inout) :: indiv

    integer(kind=stdIntKind) :: nextAge
    real(kind=stdRealKind) :: verhulstWeight
    real(kind=stdRealKind) :: verhulstFactor
    real(kind=stdRealKind) :: random

    nextAge = indiv%age + 1                     ! Hypothetical age
    verhulstWeight = MODEL_VERHULST_W(nextAge)  ! Verhulst weight per age

    ! ***Death check: Old age
    if (nextAge > MODEL_L) then
      indiv%deathIndex = DEAD_OLD_AGE
      indexOffset = indexOffset - 1
      return
    end if

    if (getBinDigit(indiv%genome, nextAge) == GENE_UNHEALTHY) then
      indiv%mutationCount = indiv%mutationCount + 1
    end if

    ! ***Death check: Mutation accumulation
    if (indiv%mutationCount >= MODEL_T) then
      indiv%deathIndex = DEAD_MUTATION
      indexOffset = indexOffset - 1
      return
    ! ***Death check: Verhulst factor
    else if (verhulstWeight > 0.0) then
      ! Get Verhulst factor per age.
      call random_number(random)
      verhulstFactor = 1 - (popSize/MODEL_K)*verhulstWeight

      if (random > verhulstFactor) then
        indiv%deathIndex = DEAD_VERHULST
        indexOffset = indexOffset - 1
        return
      end if
    end if
  end subroutine checkDeath


  function getBinDigit(number, k) result(bit)
    implicit none
    integer(kind=stdIntKind), intent(in) :: number
    integer(kind=stdIntKind), intent(in) :: k
    integer(kind=stdIntKind) :: bit

    bit = iand(shiftr(number, k - 1), 1)
  end function getBinDigit


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkBirth
  !>  Check whether `indiv` will reproduce at the current time step.
  ! -------------------------------------------------------------------------- !
  subroutine checkBirth(indiv, index, popSize, popArray, indexOffset)
    implicit none
    type(Person), allocatable, intent(inout) :: popArray(:)
    integer, intent(inout)                   :: indexOffset
    type(Person), intent(in) :: indiv
    integer, intent(in)      :: index
    integer, intent(in)      :: popSize
    integer :: i    ! Counters have default kind.

    ! Check for valid age.
    if (MODEL_R > indiv%age .or. indiv%age > MODEL_R_MAX) return

    if (popSize + MODEL_B > size(popArray)) then
      print *, "***Population size will exceed the initially allotted size."
      call changePopArraySize(popArray, MODEL_B) 
    end if

    ! Add new born indivs to the next generation.
    do i = 1, MODEL_B
      call initializeIndiv(popArray(index + indexOffset), indiv%genome)
      indexOffset = indexOffset + 1
    end do
  end subroutine checkBirth


  subroutine changePopArraySize(popArray, sizeChange)
    implicit none
    type(Person), allocatable, intent(inout) :: popArray(:)
    integer, intent(in) :: sizeChange

    type(Person), allocatable :: temp(:)
    integer :: oldSize

    oldSize = size(popArray)
    call move_alloc(popArray, temp)

    allocate(popArray(oldSize + sizeChange))
    popArray(1:oldSize) = temp(:)
    deallocate(temp)
  end subroutine changePopArraySize


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeHealthyIndiv
  !>  Initialize `indiv` with a healthy genome.
  ! -------------------------------------------------------------------------- !
  subroutine initializeHealthyIndiv(indiv)
    implicit none
    type(Person), intent(inout) :: indiv

    indiv%genome = GENE_HEALTHY
    call initializeScalarAttrs(indiv)
  end subroutine initializeHealthyIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeIndiv
  !>  Initialize `indiv` with genes inhereted from another `Person` type.
  ! -------------------------------------------------------------------------- !
  subroutine initializeIndiv(indiv, genome)
    implicit none
    type(Person), intent(inout) :: indiv
    integer(kind=stdIntKind), intent(in) :: genome
    integer(kind=stdIntKind) :: mutations(MODEL_M)
    integer :: i    ! Counters have default kind.

    call generateIndices(1_stdIntKind, int(MODEL_L, kind=stdIntKind), &
        mutations)

    indiv%genome = genome
    do i = 1, size(mutations)
      if (getBinDigit(indiv%genome, mutations(i)) == GENE_HEALTHY) then
        ! indiv%genome = indiv%genome + 2**(mutations(i) - 1)
        indiv%genome = ior(indiv%genome, shiftl(1, mutations(i) - 1))
      end if
    end do

    call initializeScalarAttrs(indiv)
  end subroutine initializeIndiv


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeScalarAttrs
  !>  Assign initial values to the scalar attributes of `indiv`.
  ! -------------------------------------------------------------------------- !
  subroutine initializeScalarAttrs(indiv)
    implicit none
    type(Person), intent(inout) :: indiv

    indiv%age = 0
    indiv%mutationCount = 0
    indiv%deathIndex = ALIVE
  end subroutine initializeScalarAttrs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: generatePopulation
  !>  Generate population of `startPopSize` size.
  ! -------------------------------------------------------------------------- !
  subroutine generatePopulation(population, startPopSize)
    implicit none
    type(Person), intent(inout) :: population(:)
    integer, intent(in) :: startPopSize   ! Counters have default kind.
    integer :: i                          ! Counters have default kind.

    do i = 1, startPopSize
      call initializeHealthyIndiv(population(i))
    end do
  end subroutine generatePopulation
end module Pop
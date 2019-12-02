module PersonType
  ! -------------------------------------------------------------------------- !
  ! MODULE:  PersonType
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing the `Person` derived type.
  ! -------------------------------------------------------------------------- !
  use ModelParam, only: MODEL_L
  use StdKind, only: personIntKind, personRealKind
  implicit none
  private

  ! Module integer and real kinds
  ! Note: Can be changed when this module is to be reused in other projects.
  integer, public, parameter :: personIK = personIntKind
  integer, public, parameter :: personRK = personRealKind

  type, public :: Person
    integer(kind=personIK) :: age
    integer(kind=personIK) :: genome
    integer                :: deathIndex     ! Only counters
    integer                :: mutationCount  ! Only counters

    type(Person), pointer  :: next => null() ! Next node in linked list
  end type Person


  ! Linked-list derived type. Structure containing pointers to the actual list.
  type, public :: LinkedList
    type(Person), pointer :: head_ptr => null()
    type(Person), pointer :: tail_ptr  => null()
    type(Person), pointer :: newTail_ptr  => null()
  end type LinkedList
end module PersonType

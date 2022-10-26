module AbstractPopulation
  ! -------------------------------------------------------------------------- !
  ! MODULE:  AbstractPopulation
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing abstract derived types for handling population and
  !!  individuals in it. These derived types serve as implementation interface
  !!  or "contracts" different implementations of population handling derived
  !!  types have to follow (if there are multiple ones).
  ! -------------------------------------------------------------------------- !
  implicit none


  type, abstract :: AbstractPerson_t
  end type AbstractPerson_t


  type, abstract :: AbstractPopulation_t
  contains
    procedure(getPopSize_abstract),     deferred :: getPopSize
    procedure(getCurrPerson_abstract),  deferred :: getCurrPerson
    procedure(atEndOfPop_abstact),      deferred :: atEndOfPopulation
    procedure(next_abstract),           deferred :: next
    procedure(startCurrStep_abstract),  deferred :: startCurrStep
    procedure(endCurrStep_abstract),    deferred :: endCurrStep
    procedure(evalCurrPerson_abstract), deferred :: evalCurrPerson
    procedure(cleanup_abstract),        deferred :: cleanup
  end type AbstractPopulation_t


  ! "Template" procedures for deferred bound procedures.
  interface
    module function getPopSize_abstract(self) result(popsize)
      class(AbstractPopulation_t), intent(in) :: self
      integer :: popSize
    end function getPopSize_abstract

    module function getCurrPerson_abstract(self) result(currPerson)
      class(AbstractPopulation_t), intent(in) :: self
      class(AbstractPerson_t), pointer :: currPerson
    end function getCurrPerson_abstract

    module subroutine next_abstract(self)
      class(AbstractPopulation_t), intent(inout) :: self
    end subroutine next_abstract

    module subroutine evalCurrPerson_abstract(self)
      class(AbstractPopulation_t), intent(inout) :: self
    end subroutine evalCurrPerson_abstract

    module function atEndOfPop_abstact(self) result(atEndOfPop)
      class(AbstractPopulation_t), intent(in) :: self
      logical :: atEndOfPop
    end function atEndOfPop_abstact

    module subroutine startCurrStep_abstract(self)
      class(AbstractPopulation_t), intent(inout) :: self
    end subroutine startCurrStep_abstract

    module subroutine endCurrStep_abstract(self)
      class(AbstractPopulation_t), intent(inout) :: self
    end subroutine endCurrStep_abstract

    module subroutine cleanup_abstract(self)
      class(AbstractPopulation_t), intent(inout) :: self
    end subroutine cleanup_abstract
  end interface
end module AbstractPopulation

module Gene
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Gene
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing gene parameters and procedures for generating
  !!  genomes.
  ! -------------------------------------------------------------------------- !
  use StdKind, only: personIK => personIntKind
  implicit none

  ! Made private so as it would not be visible in other modules/subprograms.
  private :: personIK

  integer(kind=personIK), parameter :: GENE_HEALTHY = 0
  integer(kind=personIK), parameter :: GENE_UNHEALTHY = 1
  integer(kind=personIK), parameter :: GENE_GENES(2) = &
      [GENE_HEALTHY, GENE_UNHEALTHY]
contains

  ! -------------------------------------------------------------------------- !
  ! FUNCTION:  Gene_randomGene
  !>  Generate a random gene. There is (should be) 1/2 probability of
  !!  giving a healthy or an unhealthy gene.
  ! -------------------------------------------------------------------------- !
  function Gene_randomGene() result(gene)
    implicit none

    integer(kind=personIK) :: gene
    integer                     :: randIndex
    real                        :: rand
    
    call random_number(rand)

    randIndex = floor(rand*2) + 1
    gene = GENE_GENES(randIndex)
  end function Gene_randomGene


  ! -------------------------------------------------------------------------- !
  ! FUNCTION:  gene_MutatedGene
  !>  Generate a mutated gene. As of now, it only returns unhealthy
  !!  genes.
  ! -------------------------------------------------------------------------- !
  function gene_MutatedGene() result(gene)
    implicit none
    integer :: gene

    gene = GENE_UNHEALTHY
  end function Gene_MutatedGene

  
  ! -------------------------------------------------------------------------- !
  ! FUNCTION:  gene_generateGenome
  !>  Generate an array of genes of length `L`.
  ! -------------------------------------------------------------------------- !
  function gene_generateGenome(L)
    implicit none

    integer, intent(in)         :: L
    integer(kind=personIK) :: gene_generateGenome(L)
    integer                     :: i

    gene_generateGenome = [(GENE_HEALTHY, i = 1, L)]
  end function gene_generateGenome
end module Gene

module Gene
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Gene
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing gene parameters and procedures for generating genomes.
  ! -------------------------------------------------------------------------- !
  use StdKind, only: personIntKind
  implicit none

  integer(kind=personIntKind), parameter :: GENE_HEALTHY = 0
  integer(kind=personIntKind), parameter :: GENE_UNHEALTHY = 1
  integer(kind=personIntKind), parameter :: GENE_GENES(2) = &
      [GENE_HEALTHY, GENE_UNHEALTHY]
contains

  ! -------------------------------------------------------------------------- !
  ! FUNCTION:  Gene_randomGene
  !>  Generate a random gene. There is (should be) 1/2 probability of giving
  !   a healthy or an unhealthy gene.
  ! -------------------------------------------------------------------------- !
  function Gene_randomGene() result(gene)
    integer :: randIndex
    integer(kind=personIntKind) :: gene
    real    :: rand
    
    call random_number(rand)

    randIndex = floor(rand*2) + 1
    gene = GENE_GENES(randIndex)
  end function Gene_randomGene


  ! -------------------------------------------------------------------------- !
  ! FUNCTION:  gene_MutatedGene
  !>  Generate a mutated gene. As of now, it only returns unhealthy genes.
  ! -------------------------------------------------------------------------- !
  function gene_MutatedGene() result(gene)
    integer :: gene
    gene = GENE_UNHEALTHY
  end function Gene_MutatedGene

  
  ! -------------------------------------------------------------------------- !
  ! FUNCTION:  gene_generateGenome
  !>  Generate an array of genes of length `L`.
  ! -------------------------------------------------------------------------- !
  function gene_generateGenome(L)
    integer, intent(in) :: L
    integer(kind=personIntKind) :: gene_generateGenome(L)
    integer :: i
    gene_generateGenome = [(GENE_HEALTHY, i = 1, L)]
  end function gene_generateGenome
end module Gene

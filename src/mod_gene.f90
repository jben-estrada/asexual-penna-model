module Gene
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Gene
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing gene parameters and procedures for generating
  !!  genomes.
  ! -------------------------------------------------------------------------- !
  use, intrinsic :: iso_fortran_env, only: personIK => int64, personRK => real64
  implicit none

  integer(kind=personIK), parameter :: GENE_HEALTHY = 0
    !! Integer representation of healthy genes.
  integer(kind=personIK), parameter :: GENE_UNHEALTHY = 1
    !! Integer representation of unhealthy genes.
  integer(kind=personIK), parameter :: GENE_GENES(2) = &
      [GENE_HEALTHY, GENE_UNHEALTHY]
    !! Array of 'gene' integers. Used for getting random genes.
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getGene
  !>  Get the `k`th gene (bit) of the integer `genome`.
  ! -------------------------------------------------------------------------- !
  pure function getGene(genome, k) result(gene)
    integer(kind=personIK), intent(in) :: genome
      !! A word or a bit-array represented as an integer.
    integer,                intent(in) :: k
      !! The position of the bit (gene) to obtain. Starts with 1.
    integer(kind=personIK) :: gene

    gene = 0
    gene = iand(shiftr(genome, k - 1), 1_personIK)
  end function getGene
end module Gene

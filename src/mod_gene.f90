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
end module Gene

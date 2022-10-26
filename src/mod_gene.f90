module Gene
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Gene
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing gene parameters and procedures for generating
  !!  genomes.
  ! -------------------------------------------------------------------------- !
  implicit none

  logical, parameter :: GENE_HEALTHY = .false.
    !! Integer representation of healthy genes.
  logical, parameter :: GENE_UNHEALTHY = .true.
    !! Integer representation of unhealthy genes.
end module Gene

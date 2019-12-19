module Flag
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Flag
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing `flags` for indicating an individual's state.
  ! -------------------------------------------------------------------------- !
  implicit none

  integer, parameter :: ALIVE = 1
  integer, parameter :: DEAD_OLD_AGE = 2
  integer, parameter :: DEAD_MUTATION = 3
  integer, parameter :: DEAD_VERHULST = 4
  integer, parameter :: DEATH_REASONS(4) = &
      [ALIVE,        &
      DEAD_OLD_AGE,  &
      DEAD_MUTATION, &
      DEAD_VERHULST]
end module Flag

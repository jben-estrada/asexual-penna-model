module RNG
  ! -------------------------------------------------------------------------- !
  ! MODULE: RNG
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing procedures for setting and using various RNGs.
  ! -------------------------------------------------------------------------- !
  use mtmod
  implicit none
  private

  integer, public, parameter :: RNG_INTRINSIC = 0         ! a KISS PRNG.
  integer, public, parameter :: RNG_MERSENNE_TWISTER = 1  ! MT19937 PRNG.
  integer, public, parameter :: RNG_FLAGS(2) = &
      [RNG_INTRINSIC, RNG_MERSENNE_TWISTER]

  integer, save :: rngChoice

  public :: RNG_getCmdArgs
  public :: getRandNumber
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeRNG
  !>  Initialize the chosen random number generator
  ! -------------------------------------------------------------------------- !
  subroutine RNG_getCmdArgs()
    use CmdOptions, only: rngChoiceArg, rngSeedArg
    implicit none


    call chooseRNG(rngChoiceArg % getValue())
    call setSeed(rngSeedArg % getValue())
  end subroutine RNG_getCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: chooseRNG
  !>  Choose random number generator. So far, there are two RNGs available:
  !!  the intrinsic RNG (a KISS PRNG) and a Mersenne Twister RNG (MT19937).
  ! -------------------------------------------------------------------------- !
  subroutine chooseRNG(choice)
    implicit none
    integer, intent(in) :: choice

    if (any(RNG_FLAGS == choice)) then
      rngChoice = choice
    else
      print "(a)", "***ERROR. Invalid RNG choice."
      stop
    end if
  end subroutine chooseRNG


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setSeed
  !>  Set the seed for the chosen RNG.
  ! -------------------------------------------------------------------------- !
  subroutine setSeed(seed)
    implicit none
    integer, intent(in) :: seed

    ! Local variables for setting seed of the instrinsic RNG.
    integer :: seedSize
    integer, allocatable :: intrscSeed(:)
  
    select case (rngChoice)
      ! Intrinsic KISS PRNG.
      case (RNG_INTRINSIC)
        call random_seed(size=seedSize)

        allocate(intrscSeed(seedSize))
        intrscSeed(:) = seed

        call random_seed(put=intrscSeed)

      ! External MT19937 PRNG.
      case (RNG_MERSENNE_TWISTER)
        call sgrnd(seed)
      
      case default
        print "(a)", "***ERROR. No RNG has yet been chosen."
        stop
    end select

    if (allocated(intrscSeed)) deallocate(intrscSeed)
  end subroutine setSeed


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getRandNumber
  !>  Get a random real number between 0 and 1. The exact range is RNG-
  !!  specific.
  ! -------------------------------------------------------------------------- !
  subroutine getRandNumber(randnum)
    implicit none
    real, intent(out) :: randnum
    
    select case(rngChoice)
      ! KISS PRNG
      case (RNG_INTRINSIC)
        call random_number(randnum)

      ! MT19937 PRNG
      case (RNG_MERSENNE_TWISTER)
        randnum = int(grnd()) ! Cast from real(8) to real(4) for compatibility.

      case default
        print "(a)", "***ERROR. No RNG has yet been chosen."
        stop
    end select
  end subroutine getRandNumber
end module RNG
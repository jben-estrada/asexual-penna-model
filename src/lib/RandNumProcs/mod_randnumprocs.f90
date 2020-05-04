module RandNumProcs
  ! -------------------------------------------------------------------------- !
  ! MODULE: RandNumProcs
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing procedures for setting and using various RNGs. This
  !!  module also contains various procedures for generating random integers
  !!  and real values.
  ! -------------------------------------------------------------------------- !
  use Mtmod, only: grnd, sgrnd
  use ErrorMSG, only: raiseError
  implicit none
  private

  integer, parameter :: RNG_INTRINSIC = 0
    !! FLag corresponding to the intrinsic pseudo-RNG of Fortran.
  integer, parameter :: RNG_MERSENNE_TWISTER = 1
    !! Flag corresponding to the Mersenne Twister MT19937 pseudo-RNG.
  integer, parameter :: RNG_FLAGS(2) = &
      [RNG_INTRINSIC, RNG_MERSENNE_TWISTER] !! Array of RNG flags.

  integer :: rngChoice
    !! The RNG flag chosen at the beginning of the program.

  public :: RNG_INTRINSIC
  public :: RNG_MERSENNE_TWISTER  

  public :: assignRNGParams
  public :: getRandInt
  public :: getRandReal
  public :: getRandRange
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeRNG
  !>  Initialize the chosen random number generator
  ! -------------------------------------------------------------------------- !
  subroutine assignRNGParams(rngFlag, rngSeed)
    integer, intent(in) :: rngFlag
    integer, intent(in) :: rngSeed

    call chooseRNG(rngFlag)
    call setSeed(rngSeed)
  end subroutine assignRNGParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: chooseRNG
  !>  Choose random number generator. So far, there are two RNGs available:
  !!  the intrinsic RNG (a KISS PRNG) and a Mersenne Twister RNG (MT19937).
  ! -------------------------------------------------------------------------- !
  subroutine chooseRNG(choice)
    integer, intent(in) :: choice
      !! RNG flag.

    if (any(RNG_FLAGS == choice)) then
      rngChoice = choice
    else
      call raiseError("Invalid RNG choice.")
    end if
  end subroutine chooseRNG


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setSeed
  !>  Set the seed for the chosen RNG.
  ! -------------------------------------------------------------------------- !
  subroutine setSeed(seed)
    integer, intent(in) :: seed
      !! Seed for the chosen RNG represented by `rngChosen` of `RNG` module.

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
        call raiseError("No RNG has yet been chosen.")
    end select

    if (allocated(intrscSeed)) deallocate(intrscSeed)
  end subroutine setSeed


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getRandReal
  !>  Get a random real number between 0 and 1. The exact range is RNG-
  !!  specific.
  ! -------------------------------------------------------------------------- !
  real function getRandReal()
    select case(rngChoice)
      ! KISS PRNG
      case (RNG_INTRINSIC)
        call random_number(getRandReal)

      ! MT19937 PRNG
      case (RNG_MERSENNE_TWISTER)
        ! Cast from real(8) to real(4) for compatibility.
        getRandReal = real(grnd())

      case default
        call raiseError("No RNG has yet been chosen.")
    end select
  end function getRandReal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getRandRange
  !>  Generate a rank-1 array of unique integers in the interval
  !!  [lower, upper]. The length of the generated array is that of
  !!  the passed `indices`.
  ! -------------------------------------------------------------------------- !
  function getRandRange(lower, upper, size) result(indices)
    integer, intent(in) :: upper
      !! The inclusive upper bound of integers to be randomly generated.
    integer, intent(in) :: lower
      !! The inclusive lower bound of integers to be randomly generated.
    integer, intent(in) :: size
      !! Size of the integer array to be generated.

    integer :: indices(size)
      !! The array of random integers to be used as randomized indices.
    
    integer :: index = 0
    real    :: random = 0
    integer :: i

    indices(:) = lower - 1
    do i = 1, size
      do
        random = getRandReal()
        index = getRandInt(upper, lower)

        if (all(indices(1:i) /= index)) then
          indices(i) = index
          exit
        end if

      end do
    end do
  end function getRandRange


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getRandInt
  !>  Generate a random integer in the interval [a, b].
  ! -------------------------------------------------------------------------- !
  integer function getRandInt(a, b)
    integer, intent(in) :: a
      !! The inclusive upper bound of the integer to be randomly generated.
    integer, intent(in) :: b
      !! The inclusive lower bound of the integer to be randomly generated.

    real :: random
  
    random = getRandReal()
    getRandInt = floor(random*(b - a + 1)) + a
  end function getRandInt
end module RandNumProcs

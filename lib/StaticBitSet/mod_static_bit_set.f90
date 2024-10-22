module StaticBitSetType
  ! -------------------------------------------------------------------------- !
  ! MODULE: StaticBitSetType
  ! -------------------------------------------------------------------------- !
  ! Author: John Benedick Estrada
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing an implementation of a static bit set.
  ! -------------------------------------------------------------------------- !
  use iso_fortran_env, only: logical_kinds
  use CastProcs, only: castIntToChar
  use ErrorMSG, only: raiseError
  implicit none
  private

  integer, parameter :: bitSetKind = logical_kinds(1)
  
  type :: StaticBitSet
    private
    logical(kind=bitSetKind), allocatable :: data(:)
    integer :: size
  contains
    ! Transformational procedures
    procedure :: set => bitSet_set
      !! Set the value of an element or a range of elements in the bit set.
    procedure :: setAll => bitSet_setAll
      !! Set the value of all the elements in the bit set.
    
    ! Inquiry procedures
    procedure :: get => bitSet_get
      !! Get the value of an element in the bit set.
    procedure :: any => bitSet_any
      !! Test if at least one element is TRUE / 1.
    procedure :: all => bitSet_all
      !! Test if all elements are TRUE / 1.
    procedure :: count => bitSet_count
      !! Count the number of TRUE/1 in the bit set.
    procedure :: getSize => bitSet_getSize
      !! Get the size of the bit set.
    procedure :: changeSize => bitset_changeSize
      !! Change the size of the bit set.
    procedure :: print => bitSet_print
      !! Print the bit set.
    final :: bitSet_finalizer
  end type StaticBitSet

  character, parameter :: LO_BIT_CHAR = "0"
  character, parameter :: HI_BIT_CHAR = "1"

  interface StaticBitSet
    module procedure :: bitset_cnstrc
  end interface

  interface operator(==)
    module procedure :: cmpr_bitsets
  end interface

  public :: StaticBitSet
  public :: operator(==)
contains
    

  function bitset_cnstrc(setSize, initValue) result(newBitSet)
    integer,           intent(in) :: setSize
    logical, optional, intent(in) :: initValue
    type(StaticBitSet) :: newBitSet

    if (setSize <= 0) then
      call raiseError("Invalid BitSet size: " // castIntToChar(setSize))
    end if

    newBitSet%size = setSize
    allocate(newBitSet%data(setSize))

    if (present(initValue)) newBitSet%data(:) = initValue
  end function bitset_cnstrc


  subroutine bitset_changeSize(self, newSetSize, padding)
    class(StaticBitSet), intent(inout) :: self
    integer,             intent(in) :: newSetSize
    logical, optional,   intent(in) :: padding

    logical(kind=bitSetKind), allocatable :: temp_arr(:)

    if (newSetSize == self%size) return

    call move_alloc(self%data, temp_arr)
    allocate(self%data(newSetSize))

    if (newSetSize < self%size) then
      ! Discard the trailing bits
      self%data(1:newSetSize) = temp_arr(1:newSetSize)
    else
      self%data(1:self%size) = temp_arr(1:self%size)

      if (present(padding)) then
        self%data(self%size + 1: newSetSize) = padding
      end if
    end if

    self%size = newSetSize
    deallocate(temp_arr)
  end subroutine bitset_changeSize


  subroutine bitSet_set(self, value, lo, hi)
    class(StaticBitSet), intent(inout) :: self
    logical,       intent(in)    :: value
    integer,       intent(in)    :: lo
    integer,       intent(in)    :: hi

    if (lo > hi) then
      call raiseError(  &
          "Invalid index range for StaticBitSet: " // castIntToChar(lo) //  &
          ", " //  castIntToChar(hi)  &
        )
    else if (lo < 1 .or. lo > self%size) then
      call raiseError(  &
          "Lower StaticBitSet index out of bounds: " //  &
          castIntToChar(lo)  &
        )
    else if (hi < 1 .or. hi > self%size) then
      call raiseError(  &
          "Upper StaticBitSet index out of bounds: " // &
          castIntToChar(hi)  &
        )
    end if

    self%data(lo:hi) = value
  end subroutine bitSet_set


  subroutine bitSet_setAll(self, value)
    class(StaticBitSet), intent(inout) :: self
    logical,       intent(in)    :: value
    self%data(:) = value
  end subroutine bitSet_setAll


  logical function bitSet_get(self, index) result(value)
    class(StaticBitSet), intent(in) :: self
    integer,       intent(in) :: index

    if (index < 1 .or. index > self%size) then
      call raiseError("StaticBitSet index out of bounds: " // castIntToChar(index))
    end if

    value = self%data(index)
  end function bitSet_get

  
  logical function bitSet_any(self) result(res)
    class(StaticBitSet), intent(in) :: self
    res = any(self%data(:))
  end function bitSet_any


  logical function bitSet_all(self) result(res)
    class(StaticBitSet), intent(in) :: self
    res = all(self%data(:))
  end function bitSet_all


  integer function bitSet_getSize(self) result(setSize)
    class(StaticBitSet), intent(in) :: self
    setSize = self%size
  end function bitSet_getSize


  integer function bitSet_count(self) result(countNum)
    class(StaticBitSet), intent(in) :: self
    countNum = count(self%data(:))
  end function bitSet_count


  subroutine bitSet_print(self, lowBitChar, highBitChar, delimChar)
    class(StaticBitSet), intent(in)  :: self
    character,        intent(in), optional :: lowBitChar
    character,        intent(in), optional :: highBitChar
    character(len=*), intent(in), optional :: delimChar
  
    character :: hiChar, loChar, currChar
    integer :: i

    lochar = LO_BIT_CHAR
    hichar = HI_BIT_CHAR

    if (present(lowBitChar))  loChar = lowBitChar
    if (present(highBitChar)) hiChar = highBitChar

    if (loChar == hiChar) then
      call raiseError(  &
          "Characters for high and low bits in StaticBitSet are the same."  &
        )
    end if

    do i = self%size, 1, -1
      if (self%data(i)) then
        currChar = hiChar
      else
        currChar = loChar
      end if

      write(*, "(a)", advance="no") currChar

      if (i > 1 .and. present(delimChar)) then
        write(*, "(a)", advance="no") delimChar
      end if
    end do

    ! Write a new line
    write(*, "(a)")
  end subroutine bitSet_print


  logical function cmpr_bitsets(left, right) result(cmpr_res)
    type(StaticBitSet), intent(in) :: left
    type(StaticBitSet), intent(in) :: right

    if (left%size /= right%size) then
      call raiseError( &
          "Left StaticBitSet does not have the same size as " // &
          "the right StaticBitSet." &
        )
    end if

    cmpr_res = all(left%data .eqv. right%data)
  end function cmpr_bitsets


  subroutine bitSet_finalizer(self)
    type(StaticBitSet), intent(inout) :: self

    if (allocated(self%data)) deallocate(self%data)
    self%size = 0
  end subroutine bitSet_finalizer
end module StaticBitSetType
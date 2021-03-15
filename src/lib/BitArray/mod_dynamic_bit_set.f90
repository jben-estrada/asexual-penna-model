module DynamicBitSet
  ! -------------------------------------------------------------------------- !
  ! MODULE: DynamicBitSet
  ! -------------------------------------------------------------------------- !
  ! Author: John Benedick Estrada
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing an implementation of bit set.
  ! -------------------------------------------------------------------------- !
  use iso_fortran_env, only: int64, stdint => int32
  use ANSIColorCodes, only: formatChar, escCodeGreen, escCodeRed
  use CastProcs, only: castIntToChar
  use ErrorMSG, only: raiseError
  implicit none
  private

  integer, parameter :: chunkKind = int64
  integer, parameter :: CHUNK_SIZE = storage_size(1_chunkKind)
  integer, parameter :: DEF_CHUNK_ARR_SIZE = 1

  real, parameter :: GROWTH_FACTOR = 3.0/2.0

  type :: BitSet
    private
    integer(kind=chunkKind), allocatable :: chunkArr(:)
    integer :: chunkArrSize
    integer :: loIdx
    integer :: hiIdx
  contains
    ! Transformational procedures
    procedure :: set => bitSet_set
      !! Set the value of an element or a section of the bit set.
    procedure :: setAll => bitSet_setAll
      !! Set the value of all the elements.
    procedure :: flip => bitSet_flip
      !! Flip the values of the bit set in place.
    procedure :: append => bitSet_append
      !! Put an element at the end of the bit set.
    procedure :: pop => bitSet_pop
      !! Remove the last element in the bit set.
    
    ! Inquiry procedures.
    procedure :: get => bitSet_get
      !! Get the value of an element in the bit set.
    procedure :: any => bitSet_any
      !! Test if at least one bit is 1. 
    procedure :: all => bitSet_all
      !! Test if all bits are 1. 
    procedure :: none => bitSet_none
      !! Test if none of the bits are 1.
    procedure :: len => bitSet_len
      !! Get length of the bit set.
    procedure :: getBounds => bitSet_getBounds
      !! Get the upper and lower bounds of the bit set.
    procedure :: print => bitSet_print
      !! Print the whole bit set.
    procedure :: count => bitSet_count
      !! Count the 1's in the bit set.
    final :: bitSet_finalizer
      !! Deallocate allocatable attribute(s).
  end type BitSet

  interface BitSet
    module procedure :: bitset_cnstrc
  end interface

  public :: BitSet
  public :: CHUNK_SIZE
contains


  function bitset_cnstrc() result(newBitSet)
    type(BitSet) :: newBitSet

    if (allocated(newBitSet%chunkArr)) deallocate(newBitSet%chunkArr)
    allocate(newBitSet%chunkArr(DEF_CHUNK_ARR_SIZE))
    newBitSet%chunkArr(:) = 0_chunkKind

    newBitSet%chunkArrSize = DEF_CHUNK_ARR_SIZE
    newBitSet%loIdx = 1
    newBitSet%hiIdx = 0
  end function bitset_cnstrc


  subroutine resizeChunkArr(self, maxPos)
    class(BitSet), intent(inout) :: self
    integer,       intent(in)    :: maxPos
      !! The maximum element position the array is expected to set.

    integer :: newSize
    integer(kind=chunkKind), allocatable :: tempChunkArr(:)

    newSize = int(self%chunkArrSize*GROWTH_FACTOR) + 1

    if (maxPos > newSize*CHUNK_SIZE) newSize = int(maxPos/CHUNK_SIZE) + 1

    call move_alloc(self%chunkArr, tempChunkArr)
    allocate(self%chunkArr(newSize))

    self%chunkArr(:self%chunkArrSize) = tempChunkArr(:)
    self%chunkArr(self%chunkArrSize + 1:) = 0_chunkKind

    self%chunkArrSize = newSize
    deallocate(tempChunkArr)
  end subroutine resizeChunkArr


  subroutine bitSet_set(self, value, lo, hi)
    class(BitSet), intent(inout)  :: self
    logical                       :: value
      !! The value to set the specified bits with.
    integer,           intent(in) :: lo
      !! The inclusive lower index of bits whose values are to be set with
      !! `value`. If the dummy argument `hi` is not present, the dummy argument
      !! `lo` is the lower AND upper index of the bit to be set.
    integer, optional, intent(in) :: hi
      !! The upper index of bits whose values are to be set with the dummy
      !! argument `value`. If not present, `lo` is the upper index of the bit 
      !! to be set.

      integer(kind=chunkKind) :: stencil
      integer :: lo_local
      integer :: hi_local
      integer :: chunkLo  ! Lower bound of chunk array index
      integer :: chunkHi  ! Upper bound of chunk array index
      integer :: chunkBitLo  ! Lower bound of bits within [lo, hi] in a chunk.
      integer :: chunkBitHi  ! Upper bound of bits within [lo, hi] in a chunk.

      integer :: chunkIdx
      integer :: bitIdx

      lo_local = lo
      if (present(hi)) then
        hi_local = hi
      else
        hi_local = lo
      end if

      if (lo_local < self%loIdx) call raiseError("Lower index out of bounds")
      if (hi_local > self%hiIdx) self%hiIdx = hi_local

      chunkLo = int((lo_local - 1) / CHUNK_SIZE) + 1
      chunkHi = int((hi_local - 1) / CHUNK_SIZE) + 1

      if (chunkHi > self%chunkArrSize) call resizeChunkArr(self, hi_local)

      ! NOTE: `lo_local` is in 1-based indexing. Indexing bits is 0-based!
      bitIdx = lo_local - 1
      chunkArr: do chunkIdx = chunkLo, chunkHi
        ! Determine the section to be set in the current chunk.
        if ((chunkIdx - 1)*CHUNK_SIZE < bitIdx) then
          chunkBitLo = mod(bitIdx, CHUNK_SIZE)
        else
          chunkBitLo = 0
        end if
        if (chunkIdx*CHUNK_SIZE > hi_local) then
          ! NOTE: `hi_local` is in 1-based indexing.
          chunkBitHi = mod(hi_local - 1, CHUNK_SIZE)
        else
          chunkBitHi = CHUNK_SIZE - 1
        end if

        stencil = iand(shiftl(-1_chunkKind, chunkBitLo), &
                       shiftr(-1_chunkKind, CHUNK_SIZE - (chunkBitHi + 1)))

        ! Stamp the stencil onto the current chunk to set the values of its bits
        if (value) then
          self%chunkArr(chunkIdx) = ior(self%chunkArr(chunkIdx), stencil)
        else
          self%chunkArr(chunkIdx) = iand(self%chunkArr(chunkIdx), not(stencil))
        end if

        bitIdx = bitIdx + (chunkBitHi - chunkBitLo + 1)
      end do chunkArr
  end subroutine bitSet_set


  subroutine bitSet_append(self, value)
    class(BitSet), intent(inout) :: self
    logical,       intent(in)    :: value
    call self%set(value, self%hiIdx + 1)
  end subroutine bitSet_append


  subroutine bitSet_pop(self)
    class(BitSet), intent(inout) :: self

    if (self%hiIdx - 1 < 0) then
      call raiseError("Attempted to pop an element from an emty bit set.")
    end if

    self%hiIdx = self%hiIdx - 1
  end subroutine bitSet_pop


  subroutine bitSet_setAll(self, value)
    class(BitSet), intent(inout) :: self
    logical,       intent(in)    :: value
      !! The value all the bits in the bit set are to be set with.

    if (value) then
      self%chunkArr(:) = -1_chunkKind
    else
      self%chunkArr(:) = 0_chunkKind
    end if
  end subroutine bitSet_setAll


  subroutine bitSet_flip(self)
    class(BitSet), intent(inout) :: self
    self%chunkArr(:) = not(self%chunkArr(:))
  end subroutine bitSet_flip


  function bitSet_get(self, pos) result(getVal)
    class(BitSet), intent(in) :: self
    integer,       intent(in) :: pos
      !! Position of the inquired value.
    logical :: getVal

    integer :: chunkIdx
    integer :: bitIdx

    if (pos < self%loIdx .or. pos > self%hiIdx) then
      call raiseError( &
          "Bit set index " // castIntToChar(pos) // " out of bounds." &
        )
    end if

    chunkIdx = ceiling(real(pos)/CHUNK_SIZE, kind=stdint)
    bitIdx = mod(pos - 1, CHUNK_SIZE)
    getVal = ibits(self%chunkArr(chunkIdx), bitIdx, 1) == 1
  end function bitSet_get


  function bitSet_any(self) result(anyVal)
    class(BitSet), intent(in) :: self
    integer(kind=chunkKind) :: finalChunkSection
    integer :: sectionLen
    logical :: anyVal

    ! Test the non-final chunks.
    if (self%chunkArrSize > 1) then
      anyVal = any(self%chunkArr(:self%chunkArrSize-1) /= 0_chunkKind)
    else
      anyVal = .false.
    end if

    ! Test the final chunk.
    sectionLen = self%hiIdx - int(mod(self%hiIdx, CHUNK_SIZE))
    finalChunkSection = ibits(self%chunkArr(self%chunkArrSize), 0, sectionLen)
    anyVal = anyVal .or. finalChunkSection /= 0
  end function bitSet_any


  function bitSet_all(self) result(allVal)
    class(BitSet), intent(in) :: self
    integer(kind=chunkKind) :: finalChunkSection
    integer :: sectionLen
    logical :: allVal

    ! Test the non-final chunks.
    if (self%chunkArrSize > 1) then
      allVal = any(self%chunkArr(:self%chunkArrSize-1) == -1_chunkKind)
    else
      allVal = .true.
    end if

    ! Test the final chunk.
    sectionLen = int(mod(self%hiIdx, CHUNK_SIZE))
    finalChunkSection = ibits(self%chunkArr(self%chunkArrSize), 0, sectionLen)
    allVal = allVal .and. iand(finalChunkSection, -1_chunkKind) /= -1_chunkKind
  end function bitSet_all


  function bitSet_none(self) result(noneVal)
    class(BitSet), intent(in) :: self
    logical :: noneVal
    noneVal = .not.self%all()
  end function bitSet_none


  function bitSet_getBounds(self) result(boundArr)
    class(BitSet), intent(in) :: self
    integer :: boundArr(2)
    boundArr(:) = [self%loIdx, self%hiIdx]
  end function bitSet_getBounds


  function bitSet_len(self) result(lenVal)
    class(BitSet), intent(in) :: self
    integer :: lenVal
    lenVal = self%hiIdx
  end function bitSet_len


  subroutine bitSet_print(self)
    class(BitSet), intent(in) :: self
    
    integer(kind=chunkKind) :: currChunk  ! Current chunk.
    integer :: bit
    integer :: chunkIdx
    integer :: bitPerChunkIdx

    character(len=:), allocatable :: chunkStr

    chunkArr: do chunkIdx = lbound(self%chunkArr, 1), ubound(self%chunkArr, 1)
      currChunk = self%chunkArr(chunkIdx)
      chunkStr = "|"

      chunk: do bitPerChunkIdx = 0, CHUNK_SIZE - 1
        bit = int(ibits(currChunk, bitPerChunkIdx, 1))

        if ((chunkIdx - 1)*CHUNK_SIZE + bitPerChunkIdx < self%hiIdx) then
          chunkStr = chunkStr // formatChar(castIntToChar(bit), escCodeGreen)
        else
          chunkStr = chunkStr // formatChar(castIntToChar(bit), escCodeRed)
        end if
      end do chunk

      write(*, "(a)", advance="no") chunkStr
    end do chunkArr

    ! Write the final delimiter.
    write(*, "(a)") "|"
  end subroutine bitSet_print


  function bitSet_count(self) result(count)
    class(BitSet), intent(in) :: self
    integer :: count

    integer, parameter :: lookup_tble(0:15) = &
            [0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4]

    integer(chunkKind) :: currChunk  ! Current chunk
    integer(chunkKind) :: word
    integer :: sectionLen
    integer :: chunkIdx

    count = 0
    chunkArr: do chunkIdx = lbound(self%chunkArr, 1), ubound(self%chunkArr, 1)
      if (chunkIdx*CHUNK_SIZE > self%hiIdx) then
        sectionLen = int(mod(self%hiIdx, CHUNK_SIZE))
      else
        sectionLen = CHUNK_SIZE
      end if

      currChunk = ibits(self%chunkArr(chunkIdx), 0, sectionLen)

      ! Count bits in each chunk by 4-bit "words".
      chunk: do while(currChunk /= 0_chunkKind)
        word = iand(currChunk, 15_chunkKind)
        count = count + lookup_tble(word)

        currChunk = shiftr(currChunk, 4)
      end do chunk
    end do chunkArr
  end function bitSet_count


  subroutine bitSet_finalizer(self)
    type(BitSet), intent(inout) :: self
    if (allocated(self%chunkArr)) deallocate(self%chunkArr)
  end subroutine bitSet_finalizer
end module DynamicBitSet

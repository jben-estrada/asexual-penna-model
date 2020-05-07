module HashTableType
  ! ------------------------------------------------------------------------- !
  ! MODULE: HashTableType
  ! ------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing an implementation of hash table.
  ! ------------------------------------------------------------------------- !
  use, intrinsic :: iso_fortran_env, only: int64
  use ErrorMSG, only: raiseError, raiseWarning
  implicit none
  private

  type :: Mapping
    !! A derived type representing a mapping between `key` and `value`.
    character(len=:), allocatable :: key
      !! Key with which this mapping is identified.
    character(len=:), allocatable :: value
      !! The value corresponding to the `key` attribute.

    type(Mapping), pointer :: next => null()
      !! The next `Mapping` object in the slot this object would be in.
  end type Mapping


  type :: Slot
    !! A derived type for slots in hash tables. It is a chain (linked-list to be
    !! more specific) of `Mapping` objects.
    type(Mapping), pointer :: headMapping_ptr => null()
      !! Pointer to the head of the chain (i.e. linked-list) of `Mapping`
      !! objects.
  end type Slot


  type :: HashTable
    !! Hash table type.
    private
    type(Slot), allocatable :: slotArray(:)
      !! Array of slots (or buckets) containing lists of mappings.
    integer :: slotArrSize = -1
      !! Size of the hash table.
  contains
    procedure :: get => hashtable_get
      !! Get the value which corresponds to the given key.
    procedure :: set => hashtable_set
      !! Set a new mapping in the hash table. If the key already exists,
      !! reassign the value of the key with the new value.
    procedure :: delete => hashtable_delete
      !! Delete an element in the hash table.
    final :: htDestructor
      !! Free allocated attributes.
  end type HashTable


  type :: HashTableIterator
    !! `HashTable` iterator.
    private
    type(HashTable), pointer :: iteratee_ptr => null()
      !! Pointer to the `HashTable` to be iterated over.
    integer                  :: currSlotIdx = 1
      !! Current `Slot` array index.
    type(Mapping),   pointer :: currMapping_ptr => null()
      !! Current `Mapping` object in the current `Slot` object.
  contains
    procedure :: getKey => hashtableiterator_getKey
      !! Get key and proceed to the next key-value pair.
    final :: htIterDestructor
      !! Free allocated attributes.
  end type  HashTableIterator


  ! ------------------------------------------------------------------------- !
  ! `HashTable` constructor.
  interface HashTable
    module procedure :: hashtable_cnstrct
  end interface

  interface HashTableIterator
    module procedure :: hashtableiterator_cnstrct
  end interface


  ! ------------------------------------------------------------------------- !
  ! Default hash table size.
  integer, parameter :: DEF_HASHTBL_SIZE = 50

  ! A dummy character to be returned by functions that failed
  ! (e.g. failed to find a mapping with the provided key).
  character, parameter :: CHAR_VOID = char(0)

  ! Procedure statuses.
  integer, public, parameter :: STAT_OK = 0         ! Success.
  integer, public, parameter :: STAT_NOT_FOUND = 1  ! Failed to find a mapping.

  public :: HashTable
  public :: HashTableIterator
  public :: hash
contains


  ! ------------------------------------------------------------------------- !
  ! FUNCTION: hashtable_cnstrct
  !>  Constructor for the `HashTable` type.
  ! ------------------------------------------------------------------------- !
  function hashtable_cnstrct(tableSize) result(new)
    integer, optional, intent(in)    :: tableSize
      !! Size of the hash table.

    type(HashTable) :: new

    if (present(tableSize)) then
      allocate(new % slotArray(tableSize))
    else
      allocate(new % slotArray(DEF_HASHTBL_SIZE))
    end if

    new % slotArrSize = size(new % slotArray)
  end function hashtable_cnstrct


  ! ------------------------------------------------------------------------- !
  ! FUNCTION: hash
  !>  Map the character `char` to a 64-bit integer or "hash". This hash
  !!  function implements the formula found in this website:
  !!  https://cp-algorithms.com/string/string-hashing.html
  ! ------------------------------------------------------------------------- !
  integer(kind=int64) function hash(char, slotSize)
    character(len=*),    intent(in) :: char
      !! Character to be hashed.
    integer(kind=int64), intent(in) :: slotSize
      !! Size of slot array ergo size of hash table.
    integer :: i

    ! The values for `p` and `m` are found here:
    ! https://cp-algorithms.com/string/string-hashing.html
    integer(kind=int64), parameter :: p = 53_int64
    integer(kind=int64) :: p_term

    hash = 1
    p_term = 1
    do i = 1, len(char)
      hash = hash + (iachar(char(i:i)))*(p_term**(i - 1))
      hash = mod(hash, slotSize) + 1

      ! Update the current term in the polynomial.
      p_term = mod(p_term*p, slotSize)
    end do
  end function hash


  ! ------------------------------------------------------------------------- !
  ! FUNCTION: hashtable_get
  !>  Get the value which corresponds to the given character `key`.
  ! ------------------------------------------------------------------------- !
  function hashtable_get(self, key, status) result(value)
    class(HashTable),  intent(inout) :: self
      !! `HashTable` object to be searched.
    character(len=*),  intent(in)    :: key
      !! The key whose value is to be sought for. 
    integer, optional, intent(out)   :: status
      !! Status of this function. Presence of this argument prevents this
      !! function from raising an error and stopping the program.
    character(len=:), allocatable    :: value

    type(Mapping), pointer :: currMapping_ptr
    integer(kind=int64) :: slotIdx

    ! Initialize output.
    allocate(character(len=0) :: value)

    ! Find the slot which contains the mapping we seek.
    ! NOTE: We add 1 since Fortran is, by default, one-based indexing.
    slotIdx = hash(trim(key), int(self % slotArrSize, kind=int64))
    currMapping_ptr => self % slotArray(slotIdx) % headMapping_ptr

    ! Search through the list of mappings.
    ! NOTE: Apparently gfortran doesn't do tail call optimization.
    !       So we settle with a loop implementation.
    do
      if (associated(currMapping_ptr)) then
        if (currMapping_ptr % key == key) then
          value = currMapping_ptr % value
          exit

        else
          ! Proceed to the next mapping.
          currMapping_ptr => currMapping_ptr % next
        end if
      else
        if (present(status)) then
          status = STAT_NOT_FOUND
          return
        else
          call raiseError("'" // trim(key) // &
              "' is not found in the hash table.")
        end if
      end if
    end do

    ! Signify that the function succeeded in getting a value.
    if (present(status)) status = STAT_OK

    ! Nullify local pointers.
    currMapping_ptr => null()
  end function hashtable_get


  ! ------------------------------------------------------------------------- !
  ! SUBROUTINE: hashtable_set
  !>  Set a new mapping in the hash table `self`. If the passed key `key`
  !!  already exists, reassign the value of this key with the passed argument
  !!  `value`.
  ! ------------------------------------------------------------------------- !
  subroutine hashtable_set(self, key, value, status)
    class(HashTable), intent(inout)  :: self
      !! `HashTable` object to be searched.
    character(len=*), intent(in)     :: key
      !! The key whose value is to be sought for. 
    character(len=*), intent(in)     :: value
      !! The corresponding value of the mapping.
    integer, optional, intent(out)   :: status
      !! Status of this function. Presence of this argument prevents this
      !! function from raising an error and stopping the program.
  
    type(Mapping), pointer :: currMapping_ptr
    integer(kind=int64) :: slotIdx

    ! Find the slot containing the mapping we seek.
    ! NOTE: We add 1 since Fortran is, by default, one-based indexing.
    slotIdx = hash(trim(key), int(self % slotArrSize, kind=int64))
    currMapping_ptr => self % slotArray(slotIdx) % headMapping_ptr

    ! Search through the list of mappings.
    ! NOTE: Apparently gfortran doesn't do tail call optimization.
    !       So we settle with a loop implementation.
    do
      if (associated(currMapping_ptr)) then
        if (currMapping_ptr % key == key) then
          ! Update the value of the mapping once the matching key is found.
          currMapping_ptr % value = value
          exit

        else
          if (associated(currMapping_ptr % next)) then
            ! Proceed to the next mapping in the slot.
            currMapping_ptr => currMapping_ptr % next
          else
            ! Current pointer at the end of slot. Append a new mapping to it.
            currMapping_ptr % next => allocMapping(key, value)
            exit
          end if
        end if
      else
        ! Current pointer at the end of slot. Append a new mapping to it.
        self % slotArray(slotIdx) % headMapping_ptr => allocMapping(key, value)
        exit
      end if
    end do

    ! Signify that this routine succeeded.
    if (present(status)) status = STAT_OK

    ! Nullify local pointers.
    currMapping_ptr => null()
  end subroutine hashtable_set


  ! ------------------------------------------------------------------------- !
  ! FUNCTION: allocMapping
  !>  Allocate a new `Mapping` object and initialize its character attributes
  !!  with the passed arguments `key` and `value`.
  ! ------------------------------------------------------------------------- !
  function allocMapping(key, value) result(new)
    character(len=*), intent(in) :: key
      !! The key whose value is to be sought for. 
    character(len=*), intent(in) :: value
      !! The corresponding value of the mapping.
    
    type(Mapping), pointer :: new

    allocate(new)

    new % key = trim(key)
    new % value = trim(value)
  end function allocMapping


  ! ------------------------------------------------------------------------- !
  ! SUBROUTINE: htDestructor
  !>  Destructor for the `HashTable` type.
  ! ------------------------------------------------------------------------- !
  subroutine htDestructor(self)
    type(HashTable), intent(inout) :: self
      !! `HashTable` object to be modified.

    integer :: i

    ! Free all the chained mappings.   
    if (allocated(self % slotArray)) then
      do i = lbound(self % slotArray, 1), ubound(self % slotArray, 1)
        call freeSlot(self % slotArray(i) % headMapping_ptr)
      end do
      
      ! Finally free the slot array.
      deallocate(self % slotArray)
    end if

    ! Set the hash table to 'uninitialized' state.
    self % slotArrSize = -1
  end subroutine htDestructor


  ! ------------------------------------------------------------------------- !
  ! SUBROUTINE: hashtable_delete
  !>  Delete a mapping specified by the passed `key` argument in the hash 
  !!  table.
  ! ------------------------------------------------------------------------- !
  subroutine hashtable_delete(self, key, status)
    class(HashTable), intent(inout) :: self
      !! `HashTable` object to be modified.
    character(len=*), intent(in)    :: key
      !! Key whose mapping is to be removed.
    integer, optional, intent(out) :: status
      !! Status of this function. Presence of this argument prevents this
      !! function from raising an error and stopping the program.

    type(Mapping), pointer :: currMapping_ptr
    type(Mapping), pointer :: prevMapping_ptr
    integer(kind=int64) :: slotIdx

    ! Find the slot containing the mapping we seek.
    ! NOTE: We add 1 since Fortran is, by default, one-based indexing.
    slotIdx = hash(trim(key), int(self % slotArrSize, kind=int64)) + 1
    currMapping_ptr => self % slotArray(slotIdx) % headMapping_ptr
    prevMapping_ptr => null()

    do
      if (associated(currMapping_ptr)) then
        if (currMapping_ptr % key == key) then
          ! Update the previous mapping's `next` pointer.
          if (associated(prevMapping_ptr)) then
            prevMapping_ptr % next => currMapping_ptr % next
          end if

          ! Update the head of the slot if the matching mapping is the head.
          if (associated(currMapping_ptr, &
              self % slotArray(slotIdx) % headMapping_ptr)) then
            self % slotArray(slotIdx) % headMapping_ptr => currMapping_ptr %next
          end if

          ! Remove the matching mapping.
          deallocate(currMapping_ptr)
          exit
        else
          ! Proceed to the next element in the slot.
          prevMapping_ptr => currMapping_ptr
          currMapping_ptr => currMapping_ptr % next
        end if
      else
        if (present(status)) then
          ! Signify that this routine failed to find the provided key.
          status = STAT_NOT_FOUND
          return
        else 
          call raiseError("'" // trim(key) &
              // "' is not found in the hash table")
        end if
      end if
    end do

    ! Signify that this routine succeeded.
    if (present(status)) status = STAT_OK

    ! Nullify local pointers.
    currMapping_ptr => null()
    prevMapping_ptr => null()
  end subroutine hashtable_delete


  ! ------------------------------------------------------------------------- !
  ! SUBROUTINE: freeSlot
  !>  Deallocate linked-list of `Mapping` object starting from its head
  !!  which `headMapping_ptr` is pointing at.
  ! ------------------------------------------------------------------------- !
  subroutine freeSlot(headMapping_ptr)
    type(Mapping), pointer, intent(inout) :: headMapping_ptr
      !! Pointer to the head of a chain (linekd-list) of mappings.
 
    type(Mapping), pointer :: currMapping_ptr
    type(Mapping), pointer :: toBeDealloc_ptr

    currMapping_ptr => headMapping_ptr

    ! Deallocate the whole list.
    do
      if (associated(currMapping_ptr)) then
        toBeDealloc_ptr => currMapping_ptr
        currMapping_ptr => currMapping_ptr % next

        deallocate(toBeDealloc_ptr)
      else
        exit
      end if
    end do

    ! Nullify local pointers.
    currMapping_ptr => null()
    toBeDealloc_ptr => null()
  end subroutine freeSlot


  ! ------------------------------------------------------------------------- !
  ! HASH TABLE ITERATOR PROCEDURES.
  ! ------------------------------------------------------------------------- !


  ! ------------------------------------------------------------------------- !
  ! FUNCTION: hashtableiterator_cnstrct
  !>  Constructor for the  `HashTableIterator` type.
  ! ------------------------------------------------------------------------- !
  function hashtableiterator_cnstrct(iteratee_ptr) result(new)
    type(HashTable), pointer, intent(in)    :: iteratee_ptr
    !! `HashTable` object to be iterated over.

    type(HashTableIterator) :: new

    new % iteratee_ptr => iteratee_ptr
    new % currSlotIdx = 1
    new % currMapping_ptr => iteratee_ptr % slotArray(1) % headMapping_ptr
  end function hashtableiterator_cnstrct


  ! ------------------------------------------------------------------------- !
  ! FUNCTION: hashtableiterator_getKey
  !>  Iterate over the iteratee_ptr of `self` and get the next key.
  ! ------------------------------------------------------------------------- !
  function hashtableiterator_getKey(self, status) result(key)
    class(HashTableIterator), intent(inout) :: self
      !! `HashTableIterator` to be iterated over.
    integer, optional,        intent(out)   :: status
      !! Iteration status.

    character(len=:), allocatable :: key
    
    ! Initialize output.
    allocate(character(len=0) :: key)
    if (present(status)) status = 0

    ! Find the next mapping.
    do
      if (associated(self % currMapping_ptr)) then
        key = self % currMapping_ptr % key

        ! Go to the next `Mapping` object in the current slot.
        self % currMapping_ptr => self % currMapping_ptr % next
        exit

      else if (self % currSlotIdx < size(self % iteratee_ptr % slotArray)) then
        ! Go to the next slot.
        self % currSlotIdx = self % currSlotIdx + 1

        ! Start with the head of the new slot.
        self % currMapping_ptr => &
          self % iteratee_ptr % slotArray(self % currSlotIdx) % headMapping_ptr
      
      else
        ! End of iterator.
        if (present(status)) then
          status = -1
          exit
        else
          call raiseError("End of hash table iterator.")
        end if
      end if
    end do
  end function hashtableiterator_getKey


  ! ------------------------------------------------------------------------- !
  ! SUBROUTINE: htIterDestructor
  !>  Free allocated attributes of `self` and their own attributes.
  ! ------------------------------------------------------------------------- !
  subroutine htIterDestructor(self)
    type(HashTableIterator), intent(inout) :: self
      !! `HashTableIterator` to be modified.

    ! `HashTable` pointer attribute is automatically destroyed.
    self % currMapping_ptr => null()
    self % currSlotIdx = 1
  end subroutine htIterDestructor
end module HashTableType

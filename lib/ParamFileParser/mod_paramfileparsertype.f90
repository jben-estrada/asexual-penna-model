module ParamFileParserType
  ! -------------------------------------------------------------------------- !
  ! MODULE: ParamFileParserType
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing a derived type for reading parameter values from
  !!  external files.
  ! -------------------------------------------------------------------------- !
  use HashTableType, only: &
    HashTable_t,           &
    init_HashTable,        &
    HSHTBL_STAT_OK => STAT_OK
  use CastProcs, only: &
    castCharToInt,     &
    castCharToReal32,  &
    castCharToReal64,  &
    castIntToChar
  use ASCIIProcedure, only: isWhiteSpace, isAlphaNumeric, isDigit
  use ErrorMSG, only: raiseError

  use, intrinsic :: iso_fortran_env, only: real64, real32
  implicit none
  private

  type :: ParamFileParser_t
    !! A derived type for reading files listing parameters.
    private
    character(len=:), allocatable :: filePath
      !! Path to the file to be read and parsed.
    type(HashTable_t) :: keyValTable
      !! Table into which obtained parameters from file is stored.
  contains
    procedure :: readFile => paramfileparser_readFile
      !! Read the file and store the obtained parameter values.
    generic   :: getValue => &
        paramfileparser_getScalarValue_char,   &
        paramfileparser_getScalarValue_int,    &
        paramfileparser_getScalarValue_real32, &
        paramfileparser_getScalarValue_real64, &
        paramfileparser_getArrValue_int,    &
        paramfileparser_getArrValue_real32, &
        paramfileparser_getArrValue_real64
      !! Get parameter values.
    final :: destructor
      !! Free allocated atrtibutes.

    ! Specific procedures for generic ones.
    procedure, private :: paramfileparser_getScalarValue_char
    procedure, private :: paramfileparser_getScalarValue_int
    procedure, private :: paramfileparser_getScalarValue_real32
    procedure, private :: paramfileparser_getScalarValue_real64
    procedure, private :: paramfileparser_getArrValue_int
    procedure, private :: paramfileparser_getArrValue_real32
    procedure, private :: paramfileparser_getArrValue_real64
  end type ParamFileParser_t

  ! RESERVED CHARACTERS.
  ! -------------------------------------------------------------------------- !
  ! Comment character.
  character, parameter :: COMMENT = ";"
  ! Key-value separator.
  character, parameter :: KEYVAL_SEP = "="
  ! End of line character.
  character, parameter :: EOL = achar(0)

  ! -------------------------------------------------------------------------- !
  ! Interface for submodule procedures.
  interface
    module subroutine paramScalar(paramVal, scalar)
      character(len=*), intent(in)  :: paramVal
        !! Character to be inspected and casted to either integer or real.
      class(*),         intent(out) :: scalar
        !! Output scalar value.
    end subroutine paramScalar

    module subroutine paramArray(paramVal, array)
      character(len=*), intent(in)  :: paramVal
        !! Character to be inspected and casted to integer array.
      class(*),         intent(out) :: array(:)
        !! Array output. Its type must be either integer or real.
    end subroutine paramArray
  end interface

  ! -------------------------------------------------------------------------- !
  ! Maximum length of one line.
  integer, parameter :: MAX_LINE_LEN = 256

  public :: ParamFileParser_t
  public :: init_ParamFileParser
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: paramfileparser_cnstrct
  !>  Constructor for the  `ParamFileParser_t` type.
  ! -------------------------------------------------------------------------- !
  subroutine init_ParamFileParser(new, filePath)
    type(ParamFileParser_t), intent(inout) :: new
      !! New `ParamFileParser_t` object to be initialized.
    character(len=*),        intent(in)    :: filePath
      !! Path to the file to be opened and read.


    integer :: fileStat

    call init_HashTable(new % keyValTable)

    ! Check if the file exits.
    inquire(file=filePath, iostat=fileStat)
    if (fileStat /= 0) call raiseError("'" // trim(filePath) // &
        "' cannot be opened or does not exit.")

    new % filePath = trim(filePath)  ! NOTE: Automatic allocation
  end subroutine init_ParamFileParser


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readStripFile
  !>  Read the content of file `filePath` and strip its comments and white-
  !!  space characters.
  ! -------------------------------------------------------------------------- !
  subroutine readStripFile(filePath, strippedFile)
    character(len=*),              intent(in) :: filePath
      !! Path to the file to be read.
    character(len=:), allocatable, intent(out) :: strippedFile
      !! File content with whitespaces and comments stripped off.

    character(len=MAX_LINE_LEN) :: bufferChar
    character :: currChar
    integer   :: i, status, readUnit

    ! Open file for reading.
    open(newunit=readUnit, file=filePath, status="old", iostat=status)
    if (status /= 0) call raiseError("Cannot read '" // trim(filePath) // "'")

    ! Initialize output.
    if (allocated(strippedFile)) deallocate(strippedFile)
    allocate(character(len=0) :: strippedFile)

    do
      ! Read one line from file.
      read(readUnit, "(a)", iostat=status) bufferChar
      if (status /= 0) exit  ! Exit condition.

      do i = 1, len(trim(bufferChar))
        currChar = bufferChar(i:i)

        ! Ignore the characters from here on.
        if (currChar == COMMENT) exit

        ! Everything else will be appended to the character output.
        strippedFile = strippedFile // currChar
      end do

      ! Append an end-of-line character to the output.
      strippedFile = strippedFile // EOL
    end do

    close(readUnit)
  end subroutine readStripFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: storeKeyValuePair
  !>  Separate the key and value characters in `keyValChar` and store them
  !!  in the `keyValTable` attribute of the `ParamFileParser_t` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine storeKeyValuePair(self, keyValChar)
    class(ParamFileParser_t), intent(inout) :: self
      !! `ParamFileParser_t` object to be modified.
    character(len=*),         intent(in)    :: keyValChar
      !! Character from which key and value are obtained.

    character(len=:), allocatable :: key, value
  
    character :: currChar
    logical   :: isReadingKey, isAtStart
    integer   :: i

    ! Initialize local variables.
    allocate(character(len=0) :: key)
    allocate(character(len=0) :: value)
    isReadingKey = .true.  ! Reading starts from the left.
    isAtStart = .true.

    do i = 1, len(keyValChar)
      currChar = keyValChar(i:i)

      ! Evaluate the first character of the the `keyValChar` character.
      if (isAtStart) then
        ! Ignore initial whitespace characters.
        if (isWhiteSpace(currChar)) cycle

        ! Check for invalid initial characters.
        if (.not. isAlphanumeric(currChar)) then
          call raiseError("Invalid syntax in '" // self % filePath // &
              "'. Assignment statements must not start with '"     // &
              currChar // "'.")
        end if

        ! Initial char check passed.
        isAtStart = .false.

      ! Evaluate the rest of the `keyValChar` character.
      else
        ! Switch from reading the key to reading the value.
        if (currChar == KEYVAL_SEP) then
          isReadingKey = .false.
          cycle ! Skip to the next character.
        end if
      end if

      ! Append current character to appropriate character variables.
      if (isReadingKey) then
        key = key // currChar
      else
        value = value // currChar
      end if
    end do

    ! Check if a value was actually obtained.
    if (isReadingKey .or. len(value) == 0) then
      call raiseError("No value obtained for the key '" // key // "' in '" // &
          self % filePath // "'.")
    end if

    ! Store the obtained key and value to the hash table in the
    ! `ParamFileParser_t` object.
    call self % keyValTable % set(key, value)

    ! Free local allocated characters.
    deallocate(key, value)
  end subroutine storeKeyValuePair


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_readFile
  !>  Read the contents of the file `self % filePath`, and store the obtained
  !!  key-value pair or parameters in `self % keyValueTable`.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_readFile(self)
    class(ParamFileParser_t), intent(inout) :: self
      !! Reference to the `ParamFileParser_t` object.

    character(len=:), allocatable :: strippedFile
    character(len=:), allocatable :: keyValChar
    character :: currChar
    integer   :: i

    ! Get the file contents with unneeded characters stripped off.
    call readStripFile(self % filePath, strippedFile)

    ! Initialize local variable.
    allocate(character(len=0) :: keyValChar)

    do i = 1, len(strippedFile)
      currChar = strippedFile(i:i)

      ! Finish reading the current line.
      if (currChar == EOL) then
        ! Separate key and value pairs.
        if (len(keyValChar) > 0) call storeKeyValuePair(self, keyValChar)

        ! Clear temporary char.
        deallocate(keyValChar)
        allocate(character(len=0) :: keyValChar)
      else
        ! Append current char to temporary char.
        keyValChar = keyValChar // currChar
      end if
    end do
  end subroutine paramfileparser_readFile


  subroutine paramfileparser_getScalarValue_char(self, key, scalarVal, status)
    class(ParamFileParser_t),      intent(inout) :: self
      !! `ParamFileParser_t` object to be searched.
    character(len=*),              intent(in)    :: key
      !! Key with which its corresponding value is obtained.
    character(len=:), allocatable, intent(out)   :: scalarVal
      !! Scalar output.
    integer, optional,             intent(out)   :: status
      !! Status of this routine. Presence of `status` prevents this routine
      !! from being able to stop this program.

    integer :: getStat

    ! Initialize `status` output.
    if (present(status)) status = 0

    allocate(character(len=0) :: scalarVal)
    scalarVal = self % keyValTable % get(key, getStat)

    ! Handle error in this function, not from within the `HashTableType` module
    if (getStat /= HSHTBL_STAT_OK) then
      if (present(status)) then
        status = getStat
        scalarVal = ""   ! Assign some value to the output.
      else
        call raiseError( &
          "The key '" &
          // trim(key) // &
          "' is not found in the parameter listing." &
          )
      end if
    end if
  end subroutine paramfileparser_getScalarValue_char


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_getScalarValue_int
  !>  Get the scalar integer value associated with the character `key`.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_getScalarValue_int(self, key, scalarVal, status)
    class(ParamFileParser_t), intent(inout) :: self
      !! `ParamFileParser_t` object to be searched.
    character(len=*),         intent(in)    :: key
      !! Key with which its corresponding value is obtained.
    integer,                  intent(out)   :: scalarVal
      !! Scalar output.
    integer, optional,        intent(out)   :: status
      !! Status of this routine. Presence of `status` prevents this routine
      !! from being able to stop this program.

    character(len=:), allocatable :: valueChar
    integer :: getStat

    ! Initialize `status` output.
    if (present(status)) status = 0

    allocate(character(len=0) :: valueChar)
    valueChar = self % keyValTable % get(key, getStat)
    
    ! Handle error in this function, not from within the `HashTableType` module
    if (getStat /= HSHTBL_STAT_OK) then
      if (present(status)) then
        status = getStat
        scalarVal = -1    ! Assign some value to the output.
        return
      else
        call raiseError( &
          "The key '" &
          // trim(key) // &
          "' is not found in the parameter listing." &
          )
      end if
    end if

    ! Finally convert character to the desired type.
    call paramScalar(valueChar, scalarVal)
  end subroutine paramfileparser_getScalarValue_int


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_getScalarValue_real32
  !>  Get the scalar real32 value associated with the character `key`.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_getScalarValue_real32(self, key, scalarVal, status)
    class(ParamFileParser_t), intent(inout) :: self
      !! `ParamFileParser_t` object to be searched.
    character(len=*),         intent(in)    :: key
      !! Key with which its corresponding value is obtained.
    real(kind=real32),        intent(out)   :: scalarVal
      !! Scalar output.
    integer, optional,        intent(out)   :: status
      !! Status of this routine. Presence of `status` prevents this routine
      !! from being able to stop this program.

    integer :: getStat
    character(len=:), allocatable :: valueChar

    ! Initialize `status` output.
    if (present(status)) status = 0

    allocate(character(len=0) :: valueChar)
    valueChar = self % keyValTable % get(key, getStat)

    ! Handle error in this function, not from within the `HashTableType` module
    if (getStat /= HSHTBL_STAT_OK) then
      if (present(status)) then
        status = getStat
        scalarVal = -1    ! Assign some value to the output.
        return
      else
        call raiseError( &
          "The key '" &
          // trim(key) // &
          "' is not found in the parameter listing." &
          )
      end if
    end if

    ! Finally convert character to the desired type.
    call paramScalar(valueChar, scalarVal)
  end subroutine paramfileparser_getScalarValue_real32


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_getScalarValue_real64
  !>  Get the scalar real32 value associated with the character `key`.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_getScalarValue_real64(self, key, scalarVal, status)
    class(ParamFileParser_t), intent(inout) :: self
      !! `ParamFileParser_t` object to be searched.
    character(len=*),         intent(in)    :: key
      !! Key with which its corresponding value is obtained.
    real(kind=real64),        intent(out)   :: scalarVal
      !! Scalar output.
    integer, optional,        intent(out)   :: status
      !! Status of this routine. Presence of `status` prevents this routine
      !! from being able to stop this program.

    integer :: getStat
    character(len=:), allocatable :: valueChar

    ! Initialize `status` output.
    if (present(status)) status = 0

    allocate(character(len=0) :: valueChar)
    valueChar = self % keyValTable % get(key, getStat)

    ! Handle error in this function, not from within the `HashTableType` module
    if (getStat /= HSHTBL_STAT_OK) then
      if (present(status)) then
        status = getStat
        scalarVal = -1    ! Assign some value to the output.
        return
      else
        call raiseError( &
          "The key '" &
          // trim(key) // &
          "' is not found in the parameter listing." &
          )
      end if
    end if

    ! Finally convert character to the desired type.
    call paramScalar(valueChar, scalarVal)
  end subroutine paramfileparser_getScalarValue_real64


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_getArrValue_int
  !>  Get the integer array value associated with the character `key`.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_getArrValue_int(self, key, arrVal, status)
    class(ParamFileParser_t), intent(inout) :: self
      !! `ParamFileParser_t` object to be searched.
    character(len=*),         intent(in)    :: key
      !! Key with which its corresponding value is obtained.
    integer,                  intent(out)   :: arrVal(:)
      !! Output array.
    integer, optional,        intent(out)   :: status
      !! Status of this routine. Presence of `status` prevents this routine
      !! from being able to stop this program.

    character(len=:), allocatable :: valueChar
    integer :: getStat

    ! Initialize `status` output.
    if (present(status)) status = 0

    allocate(character(len=0) :: valueChar)
    valueChar = self % keyValTable % get(key, getStat)
    
    ! Handle error in this function, not from within the `HashTableType` module
    if (getStat /= HSHTBL_STAT_OK) then
      if (present(status)) then
        status = getStat
        arrVal(:) = -1    ! Assign some values to the output.
        return
      else
        call raiseError( &
          "The key '" &
          // trim(key) // &
          "' is not found in the parameter listing." &
          )
      end if
    end if
    
    ! Finally convert character to the desired type.
    call paramArray(valueChar, arrVal)
  end subroutine paramfileparser_getArrValue_int


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_getArrValue_real32
  !>  Get the real32 array value associated with the character `key`.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_getArrValue_real32(self, key, arrVal, status)
    class(ParamFileParser_t), intent(inout) :: self
      !! `ParamFileParser_t` object to be searched.
    character(len=*),         intent(in)    :: key
      !! Key with which its corresponding value is obtained.
    real(kind=real32),        intent(out)   :: arrVal(:)
      !! Output array.
    integer, optional,        intent(out)   :: status
      !! Status of this routine. Presence of `status` prevents this routine
      !! from being able to stop this program.

    character(len=:), allocatable :: valueChar
    integer :: getStat

    ! Initialize `status` output.
    if (present(status)) status = 0

    allocate(character(len=0) :: valueChar)
    valueChar = self % keyValTable % get(key, getStat)

    ! Handle error in this function, not from within the `HashTableType` module
    if (getStat /= HSHTBL_STAT_OK) then
      if (present(status)) then
        status = getStat
        arrVal(:) = -1    ! Assign some values to the output.
        return
      else
        call raiseError( &
          "The key '" &
          // trim(key) // &
          "' is not found in the parameter listing." &
          )
      end if
    end if
    
    ! Finally convert character to the desired type.
    call paramArray(valueChar, arrVal)
  end subroutine paramfileparser_getArrValue_real32


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_getArrValue_real64
  !>  Get the real32 array value associated with the character `key`.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_getArrValue_real64(self, key, arrVal, status)
    class(ParamFileParser_t), intent(inout) :: self
      !! `ParamFileParser_t` object to be searched.
    character(len=*),         intent(in)    :: key
      !! Key with which its corresponding value is obtained.
    real(kind=real64),        intent(out)   :: arrVal(:)
      !! Output array.
    integer, optional,        intent(out)   :: status
      !! Status of this routine. Presence of `status` prevents this routine
      !! from being able to stop this program.

    character(len=:), allocatable :: valueChar
    integer :: getStat

    ! Initialize `status` output.
    if (present(status)) status = 0

    allocate(character(len=0) :: valueChar)
    valueChar = self % keyValTable % get(key, getStat)

    ! Handle error in this function, not from within the `HashTableType` module
    if (getStat /= HSHTBL_STAT_OK) then
      if (present(status)) then
        status = getStat
        arrVal(:) = -1    ! Assign some values to the output.
        return
      else
        call raiseError( &
          "The key '" &
          // trim(key) // &
          "' is not found in the parameter listing." &
          )
      end if
    end if
    
    ! Finally convert character to the desired type.
    call paramArray(valueChar, arrVal)
  end subroutine paramfileparser_getArrValue_real64


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: destructor
  !>  Destructor for the `ParamFileParser_t` type.
  ! -------------------------------------------------------------------------- !
  subroutine destructor(self)
    type(ParamFileParser_t), intent(inout) :: self
      !! `ParamFileParser_t` object to be modified.

    if (allocated(self % filePath)) deallocate(self % filePath)
    ! NOTE: `HashTable_t` attribute is automatically destroyed.
  end subroutine destructor
end module ParamFileParserType

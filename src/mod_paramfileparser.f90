module ParamFileParserType
  use HashTableType, only: HashTable
  use ErrorMSG, only: raiseError
  implicit none
  private

  type :: ParamFileParser
    !! A derived type for reading files listing parameters.
    private
    character(len=:), allocatable :: filePath
      !! Path to the file to be read and parsed.
    type(HashTable) :: keyValTable
      !! Table into which obtained parameters from file is stored.
    logical :: isInit = .false.
      !! Initialization state.
  contains
    procedure :: init => paramfileparser_init
      !! Initialize the `ParamFileParser`.
    procedure :: readFile => paramfileparser_readFile
      !! Read the file and store the obtained parameter values.
    procedure :: getValue => paramfileparser_getValue
      !! Get parameter values.
    procedure :: free => paramfileparser_free
      !! Free allocated memory.
  end type

  ! RESERVED CHARACTERS.
  ! -------------------------------------------------------------------------- !
  ! Comment character.
  character, parameter :: COMMENT = ";"
  ! Key-value separator.
  character, parameter :: KEYVAL_SEP = "="
  ! End of line character.
  character, parameter :: EOL = "/"
  ! -------------------------------------------------------------------------- !

  ! Standard unit in this module for opening and reading files.
  integer, parameter :: STD_UNIT = 50
  ! Maximum length of one line.
  integer, parameter :: MAX_LINE_LEN = 256

  public :: ParamFileParser
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_init
  !>  Initialize the `ParamFileParser` object `new`.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_init(new, filePath)
    class(ParamFileParser), intent(out) :: new
      !! New `ParamFileParser` to be initialized.
    character(len=*),      intent(in)  :: filePath
      !! Path to the file to be opened and read.
  
    call new % keyValTable % init()
    new % filePath = filePath  ! NOTE: Automatic allocation
    new % isInit = .true.
  end subroutine paramfileparser_init
  

  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isWhiteSpace
  !>  Check the character `char` if it is a whitespace character or not.
  ! -------------------------------------------------------------------------- !
  logical function isWhiteSpace(char)
    character, intent(in) :: char
      !! Character to be inspected.

    integer :: asciiCode
    asciiCode = iachar(char)

    isWhiteSpace = (8 <= asciiCode .and. asciiCode <= 13) .or. asciiCode == 32
  end function isWhiteSpace


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isAlphanumeric
  !>  Check the character `char` if it is an alphanumeric character or not.
  ! -------------------------------------------------------------------------- !
  logical function isAlphanumeric(char)
    character, intent(in) :: char
      !! Character to be inspected.

    integer :: asciiCode
    asciiCode = iachar(char)

    isAlphanumeric = (48 <= asciiCode .and. asciiCode <= 57) .or. &
                     (65 <= asciiCode .and. asciiCode <= 90) .or. &
                     (97 <= asciiCode .and. asciiCode <= 122)
  end function isAlphanumeric


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
    integer   :: i, status

    ! Open file for reading.
    open(unit=STD_UNIT, file=filePath, status="old", iostat=status)
    if (status /= 0) call raiseError("Cannot read '" // trim(filePath) // "'")

    ! Initialize output.
    if (allocated(strippedFile)) deallocate(strippedFile)
    allocate(character(len=0) :: strippedFile)

    do
      ! Read one line from file.
      read(STD_UNIT, "(a)", iostat=status) bufferChar
      if (status /= 0) exit  ! Exit condition.

      do i = 1, len(trim(bufferChar))
        currChar = bufferChar(i:i)
        
        ! Ignore whitespace characters.
        if (isWhiteSpace(currChar)) cycle

        ! Ignore the characters from here on.
        if (currChar == COMMENT) exit

        ! Everything else will be appended to the character output.
        strippedFile = strippedFile // currChar
      end do

      ! Append an end-of-line character to the output.
      strippedFile = strippedFile // EOL
    end do

    close(STD_UNIT)
  end subroutine readStripFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: storeKeyValuePair
  !>  Separate the key and value characters in `keyValChar` and store them
  !!  in the `keyValTable` attribute of the `ParamFileParser` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine storeKeyValuePair(self, keyValChar)
    class(ParamFileParser), intent(inout) :: self
      !! `ParamFileParser` object to be modified.
    character(len=*),       intent(in)    :: keyValChar
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
        ! Check for invalid initial characters.
        if (.not. isAlphanumeric(currChar)) then
          call raiseError("Invalid syntax in '" // self % filePath // &
              "'. Assignment statements must not start with '"     // &
              currChar // "'.")
        end if

        ! Ignore initial whitespace characters.
        if (isWhiteSpace(currChar)) cycle

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

      ! Ignore whitespace characters.
      if (isWhiteSpace(currChar)) cycle

      ! Append current character to appropriate character variables.
      if (isReadingKey) then
        key = key // currChar
      else
        value = value // currChar
      end if
    end do

    ! Check if a value was actually obtained.
    if (isReadingKey .or. len(value) == 0) then
      call raiseError("No value obtained for the key '" // key // "' in " // &
          self % filePath // "'.")
    end if

    ! Store the obtained key and value to the hash table in the
    ! `ParamFileParser` object.
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
    class(ParamFileParser), intent(inout) :: self
      !! Reference to the `ParamFileParser` object.

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
        if (len(currChar) /= 0) call storeKeyValuePair(self, keyValChar)

        ! Clear temporary char.
        deallocate(keyValChar)
        allocate(character(len=0) :: keyValChar)
      else
        ! Append current char to temporary char.
        keyValChar = keyValChar // currChar
      end if
    end do
  end subroutine paramfileparser_readFile


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: paramfileparser_getValue
  !>  Get the parameter value associated with the character `key`.
  ! -------------------------------------------------------------------------- !
  function paramfileparser_getValue(self, key) result(value)
    class(ParamFileParser), intent(inout) :: self
      !! `ParamFileParser` object to be searched.
    character(len=*),       intent(in)    :: key
      !! Key with which its corresponding value is obtained.

    character(len=:), allocatable :: value
    value = self % keyValTable % get(key)
  end function paramfileparser_getValue


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: paramfileparser_free
  !>  Free allocated attributes.
  ! -------------------------------------------------------------------------- !
  subroutine paramfileparser_free(self)
    class(ParamFileParser), intent(inout) :: self
      !! `ParamFileParser` object to be modified.

    self % isInit = .false.
    call self % keyValTable % free()
    deallocate(self % filePath)
  end subroutine paramfileparser_free
end module ParamFileParserType

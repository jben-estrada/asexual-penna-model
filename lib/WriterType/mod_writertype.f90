module WriterType
  ! -------------------------------------------------------------------------- !
  ! MODULE: WriterType
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing the derived type `Writer_t` for writing numerical data
  !!  to external files.
  ! -------------------------------------------------------------------------- !
  use, intrinsic :: iso_fortran_env, only: &
    writeIK =>  int64,  &
    writeRK =>  real64
  use ErrorMSG,  only: raiseError, raiseWarning
  use CastProcs, only: castIntToChar
  implicit none
  private

  integer, parameter :: UNIT_NULL = -1
    !! Placeholder value for the I/O unit.

  integer, parameter :: WRITEMODE_READABLE = 0
  integer, parameter :: WRITEMODE_BINARY   = 1

  type :: Writer_t
    !! A derived type for writing numerical data to files.
    private
    character(len=:), allocatable :: filename
      !! Name of the file data are to be written on.
    integer :: unit = UNIT_NULL
      !! Unit with which the file is to be identified within the program.
    integer :: writeMode = WRITEMODE_READABLE
      !! Mode of writing. Choices are "readable"(default) and "binary".
    logical :: fileOpen = .false.
      !! Writable state.

    ! --- Attributes for readable mode --- !
    character(len=:), allocatable :: fmtInt
      !! Write format for integers.
    character(len=:), allocatable :: fmtReal
      !! Write format for real numbers (floats).
    character(len=:), allocatable :: fmtChar
      !! Write format for characters.

    ! --- Attributes for binary mode --- !
    integer :: dataLen
      !! Length of each data points written to file. This is just record size.
    integer :: colLen
      !! Number of datapoints the writer object expects to receive per call
      !! of the write subroutines.
    contains
    private
    procedure, public :: openFile => writer_openFile
      !! Open file for writing.
    procedure, public :: closeFile => writer_closeFile
      !! Close file.
    procedure, public :: IsFileOpen => writer_isFileOpen
      !! Check if the file is open for writing.
    procedure, public :: getUnit => writer_getUnit
      !! Get the unit used to access the file.
    procedure, public :: getMode => writer_getMode
      !! Get the write mode.
    generic,   public :: write => &
      writer_write_intSclr,       &
      writer_write_realSclr,      &
      writer_write_charSclr,      &
      writer_write_intArr,        &
      writer_write_realArr,       &
      writer_write_charArr
      !! Write data of either rank-0 or rank-1 to opened file.
    generic,   public :: writeBin => &
      writer_writebin_int,           &
      writer_writebin_real,          &
      writer_writebin_char
      !! Write data in binary mode.

    final :: destructor

    procedure :: writer_write_intSclr
    procedure :: writer_write_realSclr
    procedure :: writer_write_charSclr
    procedure :: writer_write_intArr
    procedure :: writer_write_realArr
    procedure :: writer_write_charArr
    procedure :: writer_writebin_int
    procedure :: writer_writebin_real
    procedure :: writer_writebin_char
  end type Writer_t

  interface init_Writer
    module procedure :: init_Writer_readable
    module procedure :: init_Writer_binary
  end interface init_Writer

  ! Write format bases.
  character(len=*), parameter :: FMT_INT_LEN = "i15"
  character(len=*), parameter :: FMT_REAL_LEN = "f15.6"
  character(len=*), parameter :: FMT_CHAR_LEN = "a"

  public :: Writer_t
  public :: init_Writer
  public :: writeIK
  public :: writeRK
  public :: WRITEMODE_READABLE
  public :: WRITEMODE_BINARY
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: init_Writer_readable
  !>  Constructor for `Writer_t` type. (Readable mode)
  ! -------------------------------------------------------------------------- !
  module subroutine init_Writer_readable(new, filename, delim)
    type(Writer_t),    intent(inout) :: new
      !! new `Writer_t` object to be initialized.
    character(len=*),  intent(in)    :: filename
      !! Name of the file to which data is to written on.
    character(len=*),  intent(in)    :: delim
      !! Delimiter between data points in a row.

    new % filename  = trim(filename)
    new % writeMode = WRITEMODE_READABLE

    new % fmtInt  = "(*(" // FMT_INT_LEN  // ",:,'" // delim // "'))"
    new % fmtReal = "(*(" // FMT_REAL_LEN // ",:,'" // delim // "'))"
    new % fmtChar = "(*(" // FMT_CHAR_LEN // ",:,'" // delim // "'))"
  end subroutine init_Writer_readable


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: init_Writer_readable
  !>  Constructor for `Writer_t` type. (Binary mode)
  ! -------------------------------------------------------------------------- !
  module  subroutine init_Writer_binary(new, filename, dataLen, colLen)
    type(Writer_t),    intent(inout) :: new
    character(len=*),  intent(in)    :: filename
    integer,           intent(in)    :: dataLen
    integer,           intent(in)    :: colLen

    new % filename  = trim(filename)
    new % writeMode = WRITEMODE_BINARY
    new % dataLen   = dataLen
    new % colLen    = colLen
  end subroutine init_Writer_binary


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_openFile
  !>  Open the file the `Writer_t` object `self` is initialized with.
  ! -------------------------------------------------------------------------- !
  subroutine writer_openFile(self)
    class(Writer_t), intent(inout) :: self
      !! `Writer_t` object to be modified.
    integer :: openStat

    if (self % writeMode == WRITEMODE_READABLE) then
      open(newunit=self % unit, file=self % filename, access="sequential", &
           action="write", status="replace", iostat=openStat)
    else
      open(newunit=self % unit, file=self % filename, access="direct",  &
           recl=self % datalen, action="write", status="replace",       &
           iostat=openStat)
    end if

    if (openStat /= 0) then
      call raiseError(  &
        "'" // self % filename // "' cannot be opened for writing." &
      )
    end if

    if (self % writeMode == WRITEMODE_BINARY) then
      ! Write the header information to the file.
      write(self % unit, rec=self % dataLen) self % dataLen
      write(self % unit, rec=self % dataLen) self % colLen
    end if

    self % fileOpen = .true.
  end subroutine writer_openFile


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_isFileOpen
  !>  Check if the file associated with the `Writer_t` object is open for
  !!  writing.
  ! -------------------------------------------------------------------------- !
  logical function writer_isFileOpen(self)
    class(Writer_t), intent(in) :: self
    writer_isFileOpen = self % fileOpen
  end function writer_isFileOpen


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: raiseWriteError
  !>  Raise an error with message specific for the writer object.
  ! -------------------------------------------------------------------------- !
  subroutine raiseWriteError(writeObj, writeStat, requiredWriteMode)
    class(Writer_t), intent(in) :: writeObj
    integer,         intent(in) :: writeStat
    integer,         intent(in) :: requiredWriteMode

    character(len=:), allocatable :: errMsg
    logical :: hasAddError

    errMsg = "Cannot write to '" // writeObj % filename // &
        "' with IO status " // castIntToChar(writeStat)

    hasAddError = .false.

    if (.not. writeObj % fileOpen) then
      errMsg = errMsg // new_line("") // "File not opened. "
      hasAddError = .true.
    end if

    if (writeObj % writeMode /= requiredWriteMode) then
      errMsg = errMsg // new_line("") // "Attempted to write with wrong mode."
      if (writeObj % writeMode == WRITEMODE_READABLE) then
        errMsg = errMsg // "Must call the 'write' subroutine. "
      else
        errMsg = errMsg // "Must call the 'writeBin' subroutine. "
      end if
      hasAddError = .true.
    end if

    if (hasAddError) then
      errMsg = errMsg // new_line("") // &
        "(NOTE: Error may be cased by more factors.)"
    end if

    call raiseError(errMsg)
  end subroutine raiseWriteError


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_intSclr
  !>  Write integer of rank-0 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_intSclr(self, scalarData)
    class(Writer_t),       intent(inout) :: self
      !! `Writer_t` object to write data on file.
    integer(kind=writeIK), intent(in)    :: scalarData
      !! Data to be written to file.
    integer :: writeStat

    write(self % unit, self % fmtInt, iostat=writeStat) scalarData
    if (writeStat /= 0) then
      call raiseWriteError(self, writeStat, WRITEMODE_READABLE)
    end if
  end subroutine writer_write_intSclr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_realSclr
  !>  Write real of rank-0 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_realSclr(self, scalarData)
    class(Writer_t),    intent(inout) :: self
      !! `Writer_t` object to write data on file.
    real(kind=writeRK), intent(in)    :: scalarData
      !! Data to be written to file.
    integer :: writeStat

    write(self % unit, self % fmtReal, iostat=writeStat) scalarData
    if (writeStat /= 0) then
      call raiseWriteError(self, writeStat, WRITEMODE_READABLE)
    end if
  end subroutine writer_write_realSclr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_charSclr
  !>  Write character of rank-1 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_charSclr(self, scalarData)
    class(Writer_t),  intent(inout) :: self
      !! `Writer_t` object to write data on file.
    character(len=*), intent(in)    :: scalarData
      !! Data to be written to file.
    integer :: writeStat

    write(self % unit, self % fmtChar, iostat=writeStat) scalarData
    if (writeStat /= 0) then
      call raiseWriteError(self, writeStat, WRITEMODE_READABLE)
    end if
  end subroutine writer_write_charSclr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_intArr
  !>  Write integer of rank-1 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_intArr(self, arrData)
    class(Writer_t),       intent(inout) :: self
      !! `Writer_t` object to write data on file.
    integer(kind=writeIK), intent(in)    :: arrData(:)
      !! Data to be written to file.
    integer :: writeStat

    write(self % unit, self % fmtInt, iostat=writeStat) arrData
    if (writeStat /= 0) then
      call raiseWriteError(self, writeStat, WRITEMODE_READABLE)
    end if
  end subroutine writer_write_intArr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_realArr
  !>  Write real of rank-1 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_realArr(self, arrData)
    class(Writer_t),    intent(inout) :: self
      !! `Writer_t` object to write data on file.
    real(kind=writeRK), intent(in)    :: arrData(:)
      !! Data to be written to file.
    integer :: writeStat

    write(self % unit, self % fmtReal, iostat=writeStat) arrData
    if (writeStat /= 0) then
      call raiseWriteError(self, writeStat, WRITEMODE_READABLE)
    end if
  end subroutine writer_write_realArr
  

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_charArr
  !>  Write character of rank-1 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_charArr(self, arrData)
    class(Writer_t),  intent(inout) :: self
      !! `Writer_t` object to write data on file.
    character(len=*), intent(in)    :: arrData(:)
      !! Data to be written to file.
    integer :: writeStat, i

    ! Write the array data with an implied loop to satisfy check requirements.
    write(self % unit, self % fmtChar, iostat=writeStat) &
        (arrData(i), i = lbound(arrData, 1), ubound(arrData, 1))

    if (writeStat /= 0) then
      call raiseWriteError(self, writeStat, WRITEMODE_READABLE)
    end if
  end subroutine writer_write_charArr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_writebin_int
  !>  Write an integer to file in binary mode.
  ! -------------------------------------------------------------------------- !
  subroutine writer_writebin_int(self, intdata)
    class(Writer_t),       intent(inout) :: self
    integer(kind=writeIK), intent(in)    :: intData
    integer :: writeStat

    write(self % unit, rec=self % dataLen, iostat=writeStat) intData
    if (writeStat /= 0) call raiseWriteError(self, writeStat, WRITEMODE_BINARY)
  end subroutine writer_writebin_int


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_writebin_real
  !>  Write a real number to file in binary mode.
  ! -------------------------------------------------------------------------- !
  subroutine writer_writebin_real(self, realData)
    class(Writer_t),    intent(inout) :: self
    real(kind=writeRK), intent(in)    :: realData
    integer :: writeStat

    write(self % unit, rec=self % dataLen, iostat=writeStat) realData
    if (writeStat /= 0) call raiseWriteError(self, writeStat, WRITEMODE_BINARY)
  end subroutine writer_writebin_real


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_writebin_int
  !>  Write a string to file in binary mode.
  ! -------------------------------------------------------------------------- !
  subroutine writer_writebin_char(self, charData)
    class(Writer_t),  intent(inout) :: self
    character(len=*), intent(in)    :: charData
    integer :: writeStat

    write(self % unit, rec=self % dataLen, iostat=writeStat) charData
    if (writeStat /= 0) call raiseWriteError(self, writeStat, WRITEMODE_BINARY)
  end subroutine writer_writebin_char


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_getMode
  !>  Get the mode for writing.
  ! -------------------------------------------------------------------------- !
  pure integer function writer_getMode(self)
    class(Writer_t), intent(in) :: self
    writer_getMode = self % writeMode
  end function writer_getMode


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_getUnit
  !>  Get the unit used to access the file. -1 means the unit is not yet chosen.
  ! -------------------------------------------------------------------------- !
  pure integer function writer_getUnit(self)
    class(Writer_t), intent(in) :: self
    writer_getUnit = self % unit
  end function writer_getUnit


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_closeFile
  !>  Close the file associated with the `Writer_t` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_closeFile(self)
    class(Writer_t), intent(inout) :: self
      !! `Writer_t` object to be modified.

    if (.not. self % fileOpen) then
      call raiseError("Cannot close an unopened file.")
    end if

    close(self % unit)

    self % fileOpen = .false.
  end subroutine writer_closeFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: destructor
  !>  Destructor for `Writer_t` type.
  ! -------------------------------------------------------------------------- !
  subroutine destructor(self)
    type(Writer_t), intent(inout) :: self
      !! `Writer_t` object to be freed.
    if (self % fileOpen) close(self % unit)
  end subroutine destructor
end module WriterType

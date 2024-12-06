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
    writeRK =>  real64, &
    int32
  use ErrorMSG,  only: raiseError, raiseWarning
  use CastProcs, only: castIntToChar
  implicit none
  private

  integer, parameter :: UNIT_NULL = -1
    !! Placeholder value for the I/O unit.

  ! Write modes
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
    integer :: indivRecl
      !! Record length of each data points.
    integer :: colLen
      !! Number of data points per row.
    integer :: recl
      !! Actual record length, i.e. recl * colLen.
    integer :: currRec = -1
      !! The current record the data to be written to.
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

    final :: destructor

    procedure :: writer_write_intSclr
    procedure :: writer_write_realSclr
    procedure :: writer_write_charSclr
    procedure :: writer_write_intArr
    procedure :: writer_write_realArr
    procedure :: writer_write_charArr
  end type Writer_t

  interface init_Writer
    procedure :: init_Writer_readable
    procedure :: init_Writer_binary
  end interface init_Writer

  ! Write format bases.
  character(len=*), parameter :: FMT_INT_LEN  = "i15"
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
  subroutine init_Writer_readable(new, filename, delim)
    type(Writer_t),    intent(inout) :: new
      !! new `Writer_t` object to be initialized.
    character(len=*),  intent(in)    :: filename
      !! Name of the file to which data is to written on.
    character(len=*),  intent(in)    :: delim
      !! Delimiter between data points in a row.

    new%filename  = trim(filename)
    new%writeMode = WRITEMODE_READABLE

    new%fmtInt  = "(*(" // FMT_INT_LEN  // ",:,'" // delim // "'))"
    new%fmtReal = "(*(" // FMT_REAL_LEN // ",:,'" // delim // "'))"
    new%fmtChar = "(*(" // FMT_CHAR_LEN // ",:,'" // delim // "'))"
  end subroutine init_Writer_readable


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: init_Writer_readable
  !>  Constructor for `Writer_t` type. (Binary mode)
  ! -------------------------------------------------------------------------- !
  subroutine init_Writer_binary(new, filename, indivRecl, colLen)
    type(Writer_t),    intent(inout) :: new
      !! new `Writer_t` object to be initialized.
    character(len=*),  intent(in)    :: filename
      !! Name of the file to which data is to written on.
    integer,           intent(in)    :: indivRecl
      !! Record length. The size of each data points in the file.
      !! NOTE: This could vary with different compilers and compiler options.
      !!       e.g. Intel Fortran may use 1 unit : 4 bytes, while gfortran may
      !!            use 1 unit : 1 byte.
    integer,           intent(in)    :: colLen
      !! Number of data points per row. This has no bearing on the actual size
      !! of the data points in the file. This is purely for post-processing.

    if (indivRecl == 0) then
      call raiseError("Invalid record length: " // castIntToChar(indivRecl))
    end if
    if (colLen == 0) then
      call raiseError("Invalid column number: " // castIntToChar(colLen))
    end if

    new%filename  = trim(filename)
    new%writeMode = WRITEMODE_BINARY
    new%indivRecl = indivRecl
    new%colLen    = colLen
    new%recl      = indivRecl * colLen
  end subroutine init_Writer_binary


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: raiseOpenFileError
  !>  Raise an error if the file cannot be opened for writing.
  ! -------------------------------------------------------------------------- !
  subroutine raiseOpenFileError(filename, iomsg)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: iomsg
    call raiseError(  &
          "'" // filename // "' cannot be opened for writing. "  // iomsg  &
        )
  end subroutine raiseOpenFileError


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_openFile
  !>  Open the file the `Writer_t` object `self` is initialized with.
  ! -------------------------------------------------------------------------- !
  subroutine writer_openFile(self, addHeaderInfo)
    class(Writer_t),   intent(inout) :: self
      !! `Writer_t` object to be modified.
    integer, optional, intent(in)    :: addHeaderInfo(:)

    character(len=256) :: openMsg
    integer :: openStat
    integer :: int32recl, headerLen
    integer :: i

    if (self%writeMode == WRITEMODE_READABLE) then
      open(newunit=self%unit, file=self%filename, access="sequential", &
           action="write", status="replace", iostat=openStat, iomsg=openMsg)

      if (openStat /= 0) call raiseOpenFileError(self%filename, openMsg)
    else

      ! Get the record length of int32
      inquire(iolength=int32recl) 1_int32

      ! Append relevant information (1 standard record length each) to the file.
      open( &
        newunit=self%unit, file=self%filename, access="direct", &
        recl=int32recl, action="write", status="replace",       &
        iostat=openStat, iomsg=openMsg &
      )
      if (openStat /= 0) call raiseOpenFileError(self%filename, openMsg)

      ! Write the header information to the file.
      write(self%unit, rec=1) int(self%indivRecl, kind=int32)
      write(self%unit, rec=2) int(self%colLen, kind=int32)
      self%currRec = 3

      ! Append additional header information.
      if (present(addHeaderInfo)) then
        do i = lbound(addHeaderInfo, 1), ubound(addHeaderInfo, 1)
          write(self%unit, rec=self%currRec) int(addHeaderInfo(i), int32)
          self%currRec = self%currRec + 1
        end do
      end if
    
      write(self%unit, rec=self%currRec) 0_int32  ! Padding
      headerLen = self%currRec * int32recl

      close(self%unit)

      self%currRec = ceiling(real(headerLen / self%recl)) + 1

      ! Finally open the file for writing.
      open(  &
        newunit=self%unit, file=self%filename, access="direct", &
        recl=self%recl, action="write", status="old",           &
        iostat=openStat, iomsg=openMsg                          &
      )
      if (openStat /= 0) call raiseOpenFileError(self%filename, openMsg)
    end if

    self%fileOpen = .true.
  end subroutine writer_openFile


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_isFileOpen
  !>  Check if the file associated with the `Writer_t` object is open for
  !!  writing.
  ! -------------------------------------------------------------------------- !
  logical function writer_isFileOpen(self)
    class(Writer_t), intent(in) :: self
    writer_isFileOpen = self%fileOpen
  end function writer_isFileOpen


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: raiseWriteError
  !>  Raise an error with message specific for the writer object.
  ! -------------------------------------------------------------------------- !
  subroutine raiseWriteError(writeObj, writeStat)
    class(Writer_t), intent(in) :: writeObj
    integer,         intent(in) :: writeStat

    character(len=:), allocatable :: errMsg
    logical :: hasAddError

    errMsg = "Cannot write to '" // writeObj%filename // &
             "' with IO status " // castIntToChar(writeStat)

    hasAddError = .false.

    if (.not. writeObj%fileOpen) then
      errMsg = errMsg // new_line("") // "File not opened. "
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
  subroutine writer_write_intSclr(self, intScalarData)
    class(Writer_t),       intent(inout) :: self
      !! `Writer_t` object to write data on file.
    integer(kind=writeIK), intent(in)    :: intScalarData
      !! Data to be written to file.
    integer :: writeStat

    if (self%writeMode == WRITEMODE_READABLE) then
      write(self%unit, self%fmtInt, iostat=writeStat) intScalarData
    else
      write(self%unit, rec=self%currRec, iostat=writeStat) intScalarData
      self%currRec = self%currRec + 1
    end if

    if (writeStat /= 0) call raiseWriteError(self, writeStat)
  end subroutine writer_write_intSclr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_realSclr
  !>  Write real of rank-0 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_realSclr(self, realScalarData)
    class(Writer_t),    intent(inout) :: self
      !! `Writer_t` object to write data on file.
    real(kind=writeRK), intent(in)    :: realScalarData
      !! Data to be written to file.
    integer :: writeStat

    if (self%writeMode == WRITEMODE_READABLE) then
      write(self%unit, self%fmtReal, iostat=writeStat) realScalarData
    else
      write(self%unit, rec=self%currRec, iostat=writeStat) realScalarData
      self%currRec = self%currRec + 1
    end if

    if (writeStat /= 0) call raiseWriteError(self, writeStat)
  end subroutine writer_write_realSclr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_charSclr
  !>  Write character of rank-1 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_charSclr(self, charScalarData)
    class(Writer_t),  intent(inout) :: self
      !! `Writer_t` object to write data on file.
    character(len=*), intent(in)    :: charScalarData
      !! Data to be written to file.
    integer :: writeStat

    if (self%writeMode == WRITEMODE_READABLE) then
      write(self%unit, self%fmtChar, iostat=writeStat) charScalarData
    else
      write(self%unit, rec=self%currRec, iostat=writeStat) charScalarData
      self%currRec = self%currRec + 1
    end if

    if (writeStat /= 0) call raiseWriteError(self, writeStat)
  end subroutine writer_write_charSclr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_intArr
  !>  Write integer of rank-1 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_intArr(self, intArrData)
    class(Writer_t),       intent(inout) :: self
      !! `Writer_t` object to write data on file.
    integer(kind=writeIK), intent(in)    :: intArrData(:)
      !! Data to be written to file.
    integer :: writeStat

    if (self%writeMode == WRITEMODE_READABLE) then
      write(self%unit, self%fmtInt, iostat=writeStat) intArrData
    else
      write(self%unit, rec=self%currRec, iostat=writeStat) intArrData
      self%currRec = self%currRec + 1
    end if

    if (writeStat /= 0) call raiseWriteError(self, writeStat)
  end subroutine writer_write_intArr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_realArr
  !>  Write real of rank-1 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_realArr(self, realArrData)
    class(Writer_t),    intent(inout) :: self
      !! `Writer_t` object to write data on file.
    real(kind=writeRK), intent(in)    :: realArrData(:)
      !! Data to be written to file.
    integer :: writeStat

    if (self%writeMode == WRITEMODE_READABLE) then
      write(self%unit, self%fmtReal, iostat=writeStat) realArrData
    else
      write(self%unit, rec=self%currRec, iostat=writeStat) realArrData
      self%currRec = self%currRec + 1
    end if

    if (writeStat /= 0) call raiseWriteError(self, writeStat)
  end subroutine writer_write_realArr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_write_charArr
  !>  Write character of rank-1 to file associated with the `Writer_t` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_charArr(self, charArrData)
    class(Writer_t),  intent(inout) :: self
      !! `Writer_t` object to write data on file.
    character(len=*), intent(in)    :: charArrData(:)
      !! Data to be written to file.
    integer :: writeStat, i

    
    if (self%writeMode == WRITEMODE_READABLE) then
      ! Write the array data with an implied loop to satisfy check requirements.
      write(self%unit, self%fmtChar, iostat=writeStat) &
          (charArrData(i), i = lbound(charArrData, 1), ubound(charArrData, 1))
    else
      write(self%unit, rec=self%currRec, iostat=writeStat) &
        (charArrData(i), i = lbound(charArrData, 1), ubound(charArrData, 1))
      self%currRec = self%currRec + 1
    end if

    if (writeStat /= 0) call raiseWriteError(self, writeStat)
  end subroutine writer_write_charArr


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_getMode
  !>  Get the mode for writing.
  ! -------------------------------------------------------------------------- !
  pure integer function writer_getMode(self)
    class(Writer_t), intent(in) :: self
    writer_getMode = self%writeMode
  end function writer_getMode


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_getUnit
  !>  Get the unit used to access the file. -1 means the unit is not yet chosen.
  ! -------------------------------------------------------------------------- !
  pure integer function writer_getUnit(self)
    class(Writer_t), intent(in) :: self
    writer_getUnit = self%unit
  end function writer_getUnit


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_closeFile
  !>  Close the file associated with the `Writer_t` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_closeFile(self)
    class(Writer_t), intent(inout) :: self
      !! `Writer_t` object to be modified.

    if (.not. self%fileOpen) then
      call raiseError("Cannot close an unopened file.")
    end if

    close(self%unit)

    self%fileOpen = .false.
  end subroutine writer_closeFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: destructor
  !>  Destructor for `Writer_t` type.
  ! -------------------------------------------------------------------------- !
  subroutine destructor(self)
    type(Writer_t), intent(inout) :: self
      !! `Writer_t` object to be freed.
    if (self%fileOpen) close(self%unit)
  end subroutine destructor
end module WriterType

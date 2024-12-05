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
    writeIK => int64, &
    writeRK =>  real64
  use ErrorMSG, only: raiseError, raiseWarning
  implicit none
  private

  integer, parameter :: UNIT_NULL = -1
    !! Placeholder value for the I/O unit.

  type :: Writer_t
    !! A derived type for writing numerical data to files.
    private
    character(len=:), allocatable :: filename
      !! Name of the file data are to be written on.
    integer :: unit = UNIT_NULL
      !! Unit with which the file is to be identified within the program.
    logical :: fileOpen = .false.
      !! Writable state.

    character(len=:), allocatable :: fmtInt
      !! Write format for integers.
    character(len=:), allocatable :: fmtReal
      !! Write format for real numbers (floats).
    character(len=:), allocatable :: fmtChar
      !! Write format for characters.
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

  ! Write format bases.
  character(len=*), parameter :: FMT_INT_LEN = "i15"
  character(len=*), parameter :: FMT_REAL_LEN = "f15.6"
  character(len=*), parameter :: FMT_CHAR_LEN = "a"

  public :: Writer_t
  public :: init_Writer
  public :: writeIK
  public :: writeRK
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_cnstrct
  !>  Constructor for `Writer_t` type.
  ! -------------------------------------------------------------------------- !
  subroutine init_Writer(new, filename, delim, unit)
    type(Writer_t),    intent(inout) :: new
      !! new `Writer_t` object to be initialized.
    character(len=*),  intent(in)    :: filename
      !! Name of the file to which data is to written on.
    character(len=*),  intent(in)    :: delim
      !! Delimiter between data points in a row.
    integer, optional, intent(in)    :: unit
      !! Unit with which the file is identified within the program.
      !! If not provided, the program will choose the unit.

    new % filename = trim(filename)
    if (present(unit))  new % unit = unit

    new % fmtInt  = "(*(" // FMT_INT_LEN // ",:,'" // delim // "'))"
    new % fmtReal = "(*(" // FMT_REAL_LEN // ",:,'" // delim // "'))"
    new % fmtChar = "(*(" // FMT_CHAR_LEN // ",:,'" // delim // "'))"
  end subroutine init_Writer


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_openFile
  !>  Open the file the `Writer_t` object `self` is initialized with.
  ! -------------------------------------------------------------------------- !
  subroutine writer_openFile(self)
    class(Writer_t), intent(inout) :: self
      !! `Writer_t` object to be modified.
    integer :: openStat

    if (self % unit == UNIT_NULL) then
      open(newunit=self % unit, file=self % filename, iostat=openStat)
    else
      open(   unit=self % unit, file=self % filename, iostat=openStat)
    end if

    if (openStat /= 0) then
      call raiseError(  &
        "'" // self % filename // "' cannot be opened." &
      )
    end if

    self % fileOpen = .true.
  end subroutine writer_openFile


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_isFileOpen
  !>  Check if the file associated with the `Writer_t` object is open for writing.
  ! -------------------------------------------------------------------------- !
  logical function writer_isFileOpen(self)
    class(Writer_t), intent(in) :: self
    writer_isFileOpen = self % fileOpen
  end function writer_isFileOpen


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkFileState
  !>  Check if the file to be written on is open. Raises error if not.
  ! -------------------------------------------------------------------------- !
  subroutine checkFileState(writerObj, msg)
    class(Writer_t), intent(in) :: writerObj
      !! 'Writer' object to be checked.
    character(len=*), intent(in) :: msg
      !! Error message.

    if (.not. writerObj % fileOpen) then
      call raiseError( & 
          msg // " '" // writerObj % filename // "' is not yet opened." &
        )
    end if
  end subroutine checkFileState


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

    call checkFileState(self, "Cannot write to file.")


    write(self % unit, self % fmtInt, iostat=writeStat) scalarData
    if (writeStat /= 0) then
      call raiseError("Cannot write to '" // self % filename // "'.")
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

    call checkFileState(self, "Cannot write to file.")

    write(self % unit, self % fmtReal, iostat=writeStat) scalarData
    if (writeStat /= 0) then
      call raiseError("Cannot write to '" // self % filename // "'.")
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

    call checkFileState(self, "Cannot write to file.")

    write(self % unit, self % fmtChar, iostat=writeStat) scalarData
    if (writeStat /= 0) then
      call raiseError("Cannot write to '" // self % filename // "'.")
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

    call checkFileState(self, "Cannot write to file.")

    write(self % unit, self % fmtInt, iostat=writeStat) arrData
    if (writeStat /= 0) then
      call raiseError("Cannot write to '" // self % filename // "'.")
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

    call checkFileState(self, "Cannot write to file.")

    write(self % unit, self % fmtReal, iostat=writeStat) arrData
    if (writeStat /= 0) then
      call raiseError("Cannot write to '" // self % filename // "'.")
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

    call checkFileState(self, "Cannot write to file.")

    ! Write the array data with an implied loop to satisfy check requirements.
    write(self % unit, self % fmtChar, iostat=writeStat) &
        (arrData(i), i = lbound(arrData, 1), ubound(arrData, 1))

    if (writeStat /= 0) then
      call raiseError("Cannot write to '" // self % filename // "'.")
    end if
  end subroutine writer_write_charArr


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

    call checkFileState(self, "Cannot close file.")
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

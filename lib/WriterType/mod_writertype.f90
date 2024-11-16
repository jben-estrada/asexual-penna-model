module WriterType
  ! -------------------------------------------------------------------------- !
  ! MODULE: WriterType
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing the derived type `Writer` for writing numerical data
  !!  to external files.
  ! -------------------------------------------------------------------------- !
  use, intrinsic :: iso_fortran_env, only: &
    writeIK => int64, &
    writeRK =>  real64
  use ErrorMSG, only: raiseError, raiseWarning
  implicit none
  private

  type :: Writer
    !! A derived type for writing numerical data to files.
    private
    character(len=:), allocatable :: filename
      !! Name of the file data are to be written on.
    integer :: unit
      !! Unit with which the file is to be identified within the program.
    logical :: fileOpen = .false.
      !! Writable state.

    character :: delim = ""
      !! Data delimiters
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
  end type

  ! Write format bases.
  character(len=*), parameter :: FMT_INT_LEN = "i15"
  character(len=*), parameter :: FMT_REAL_LEN = "f15.6"
  character(len=*), parameter :: FMT_CHAR_LEN = "a"

  !! `Writer` constructor.
  interface Writer
    module procedure :: writer_cnstrct
  end interface

  public :: Writer
  public :: writeIK
  public :: writeRK
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_cnstrct
  !>  Constructor for `Writer` type.
  ! -------------------------------------------------------------------------- !
  function writer_cnstrct(filename, unit, delim) result(new)
    character(len=*), intent(in)  :: filename
      !! Name of the file to which data is to written on.
    integer,          intent(in)  :: unit
      !! Unit with which the file is identified within the program.
    character,        intent(in)  :: delim

    type(Writer) :: new

    new % filename = trim(filename)
    new % unit = unit
    new % delim = delim

    new % fmtInt = "(*(" // FMT_INT_LEN // ",:,'" // delim // "'))"
    new % fmtReal = "(*(" // FMT_REAL_LEN // ",:,'" // delim // "'))"
    new % fmtChar = "(*(" // FMT_CHAR_LEN // ",:,'" // delim // "'))"
  end function writer_cnstrct


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_openFile
  !>  Open the file the `Writer` object `self` is initialized with.
  ! -------------------------------------------------------------------------- !
  subroutine writer_openFile(self)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    integer :: openStat

    open(unit=self % unit, file=self % filename, iostat=openStat)
    if (openStat /= 0) then
      call raiseError(&
        "'" // self % filename // &
        "' cannot be opened or its directory does not exist." &
        )
    end if

    self % fileOpen = .true.
  end subroutine writer_openFile


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: writer_isFileOpen
  !>  Check if the file associated with the `Writer` object is open for writing.
  ! -------------------------------------------------------------------------- !
  logical function writer_isFileOpen(self)
    class(Writer), intent(in) :: self
    writer_isFileOpen = self % fileOpen
  end function writer_isFileOpen


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkFileState
  !>  Check if the file to be written on is open. Raises error if not.
  ! -------------------------------------------------------------------------- !
  subroutine checkFileState(writerObj, msg)
    class(Writer), intent(in) :: writerObj
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
  !>  Write integer of rank-0 to file associated with the `Writer` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_intSclr(self, scalarData)
    class(Writer),         intent(inout) :: self
      !! `Writer` object to write data on file.
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
  !>  Write real of rank-0 to file associated with the `Writer` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_realSclr(self, scalarData)
    class(Writer),      intent(inout) :: self
      !! `Writer` object to write data on file.
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
  !>  Write character of rank-1 to file associated with the `Writer` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_charSclr(self, scalarData)
    class(Writer),    intent(inout) :: self
      !! `Writer` object to write data on file.
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
  !>  Write integer of rank-1 to file associated with the `Writer` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_intArr(self, arrData)
    class(Writer),         intent(inout) :: self
      !! `Writer` object to write data on file.
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
  !>  Write real of rank-1 to file associated with the `Writer` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_realArr(self, arrData)
    class(Writer),      intent(inout) :: self
      !! `Writer` object to write data on file.
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
  !>  Write character of rank-1 to file associated with the `Writer` object
  !!  `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_write_charArr(self, arrData)
    class(Writer),    intent(inout) :: self
      !! `Writer` object to write data on file.
    character(len=*), intent(in)    :: arrData(:)
      !! Data to be written to file.
    integer :: writeStat

    call checkFileState(self, "Cannot write to file.")

    write(self % unit, self % fmtChar, iostat=writeStat) arrData
    if (writeStat /= 0) then
      call raiseError("Cannot write to '" // self % filename // "'.")
    end if
  end subroutine writer_write_charArr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_closeFile
  !>  Close the file associated with the `Writer` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_closeFile(self)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.

    call checkFileState(self, "Cannot close file.")
    close(self % unit)

    self % fileOpen = .false.
  end subroutine writer_closeFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: destructor
  !>  Destructor for `Writer` type.
  ! -------------------------------------------------------------------------- !
  subroutine destructor(self)
    type(Writer), intent(inout) :: self
      !! `Writer` object to be freed.
    if (self % fileOpen) close(self % unit)
  end subroutine destructor
end module WriterType

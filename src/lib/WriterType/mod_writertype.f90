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
    logical :: toAppendData = .false.
      !! Appended data 
    logical :: isInit = .false.
      !! Initialization state.
    logical :: isFileOpen = .false.
      !! Writable state.
  contains
    private
    procedure, public :: init => writer_init
      !! Initialize the 'Writer' object.
    procedure, public :: openFile => writer_openFile
      !! Open file for writing.
    procedure, public :: closeFile => writer_closeFile
      !! Close file.
    procedure, public :: free =>  writer_free
      !! Free allocated attributes and close file if it still open.
    generic,   public :: write => &
      writer_write_intSclr, &
      writer_write_realSclr, &
      writer_write_charSclr, &
      writer_write_intArr, &
      writer_write_realArr, &
      writer_write_charArr
      !! Write data of either rank-0 or rank-1 to opened file.

    procedure :: writer_write_intSclr
    procedure :: writer_write_realSclr
    procedure :: writer_write_charSclr
    procedure :: writer_write_intArr
    procedure :: writer_write_realArr
    procedure :: writer_write_charArr
  end type

  ! Write formats.
  character(len=*), parameter :: FMT_INT =  "(*(i15, '|'))"
  character(len=*), parameter :: FMT_REAL =  "(*(f15.6, '|'))"
  character(len=*), parameter :: FMT_CHAR =  "(*(a15, '|'))"

  public :: Writer
  public :: writeIK
  public :: writeRK
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_init
  !>  Initialize the `Writer` object `self`.
  ! -------------------------------------------------------------------------- !
  subroutine writer_init(self, filename, unit, toAppendData)
    class(Writer),    intent(out) :: self
      !! `Writer` object to be initialized.
    character(len=*), intent(in)  :: filename
      !! Name of the file to which data is to written on.
    integer,          intent(in)  :: unit
      !! Unit with which the file is identified within the program.
    logical,          intent(in)  :: toAppendData
      !! Append data to existing file if true. Overwrite data to exisiting file
      !! if false.
  
    self % filename = trim(filename)
    self % unit = unit
    self % toAppendData = toAppendData
    self % isInit = .true.
  end subroutine writer_init


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getWritePos
  !>  Get position specifier "append" if `toAppendData` is true. Otherwise, 
  !!  get "asis".
  ! -------------------------------------------------------------------------- !
  function getWritePos(toAppendData) result(position)
    logical, intent(in) :: toAppendData
      !! Append data to file.

    character(len=:), allocatable :: position

    allocate(character(len=0) :: position)
    
    if (toAppendData) then
      position = "append"
    else
      position = "asis"
    end if
  end function getWritePos


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_openFile
  !>  Open the file the `Writer` object `self` is initialized with.
  ! -------------------------------------------------------------------------- !
  subroutine writer_openFile(self)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    integer :: openStat

    if (.not. self % isInit) then
      call raiseError( &
        "Cannot open files. 'Writer' object is uninitialized yet." &
        )
    end if

    open(unit=self % unit, file=self % filename, iostat=openStat, &
      position=getWritePos(self % toAppendData))
    if (openStat /= 0) then
      call raiseError(&
        "'" // self % filename // &
        "' cannot be opened or does not exists." &
        )
    end if

    self % isFileOpen = .true.
  end subroutine writer_openFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkInitState
  !>  Check the initialization and writeable state of the `Writer` object
  !!  `writerObj`.
  ! -------------------------------------------------------------------------- !
  subroutine checkInitState(writerObj)
    class(Writer), intent(in) :: writerObj
      !! 'Writer' object to be checked.

    if (.not. writerObj % isInit) then
      call raiseError( &
        "Cannot write to file. 'Writer' object is uninitialized yet." &
        )
    else if (.not. writerObj % isFileOpen) then
      call raiseError( &
        "Cannot write to file. '" // writerObj % filename // &
        "' is not yet opened." &
        )
    end if
  end subroutine checkInitState


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

    call checkInitState(self)

    write(self % unit, FMT_INT, iostat=writeStat) scalarData
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

    call checkInitState(self)

    write(self % unit, FMT_REAL, iostat=writeStat) scalarData
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

    call checkInitState(self)

    write(self % unit, FMT_CHAR, iostat=writeStat) scalarData
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

    call checkInitState(self)

    write(self % unit, FMT_INT, iostat=writeStat) arrData
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

    call checkInitState(self)

    write(self % unit, FMT_REAL, iostat=writeStat) arrData
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

    call checkInitState(self)

    write(self % unit, FMT_CHAR, iostat=writeStat) arrData
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

    call checkInitState(self)
    close(self % unit)

    self % isFileOpen = .false.
  end subroutine writer_closeFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: writer_free
  !>  Free allocated attributes and close opened file if it is still open.
  ! -------------------------------------------------------------------------- !
  subroutine writer_free(self)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.

    if (.not. self % isInit) then
      call raiseWarning("'Writer' object is uninitialized.")
    end if

    if (allocated(self % filename)) deallocate(self % filename)
    if (self % isFileOpen) close(self % unit)
  end subroutine writer_free
end module WriterType

module SaveFormat
  ! -------------------------------------------------------------------------- !
  ! MODULE:  SaveFormat
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for saving data
  ! -------------------------------------------------------------------------- !
  use UpdateArray, only: arrayInsert, arrayRemoveElem
  use StdKind, only: writeIntKind, writeRealKind
  implicit none
  private

  ! -------------------------------------------------------------------------- !
  ! `Writer` derived type. A reusable unified interface for writing files.
  type, public :: Writer
    private
    integer, public, allocatable :: enabledFlags(:)
    integer,         allocatable :: liveFlags(:)
  contains
    private
    generic, public :: initialize => writer_initialize, writer_initializeAll, &
        writer_listInitialize
    generic, public :: close => writer_close, writer_closeAll, writer_listclose
    generic, public :: write => writer_write_int, writer_write_real, &
        writer_write_intArray, writer_write_realArray
    final :: destructor

    procedure :: writer_initialize
    procedure :: writer_initializeAll
    procedure :: writer_listInitialize
    procedure :: writer_close
    procedure :: writer_closeAll
    procedure :: writer_listclose
    procedure :: writer_write_int
    procedure :: writer_write_real
    procedure :: writer_write_intArray
    procedure :: writer_write_realArray
  end type
  ! -------------------------------------------------------------------------- !
  ! Number of Files to save
  integer, public, parameter :: FILECOUNT = 5
  
  ! Max character length
  integer, parameter :: MAXLEN = 32

  ! Population size record. fileCount => 1
  character(len=MAXLEN), parameter :: popFilename = "pop_size_f08.csv"
  character(len=MAXLEN), parameter :: popFormat = "(i6)"
  character(len=MAXLEN), parameter :: popPosition = "asis"
  integer, parameter :: popUnit = 100

  ! Timing record. fileCount => 2
  ! Format: <no. of time step> <no. of population> <wall time>
  character(len=MAXLEN), parameter :: timeFilename = "time_f08.csv"
  character(len=MAXLEN), parameter :: timeFormat = "(f10.5,',',f10.5,',',f10.5)"
  character(len=MAXLEN), parameter :: timePosition = "append"
  integer, parameter :: timeUnit = 101

  ! Age demographics record. fileCount => 3
  character(len=MAXLEN), parameter :: ageDstrbFilename = "ageDstrb_f08.csv"
  character(len=MAXLEN), parameter :: ageDstrbFormat = "(*(f10.5, ','))"
  character(len=MAXLEN), parameter :: ageDstrbPosition = "asis"
  integer, parameter :: ageDstrbUnit = 102

  ! Genome demographics record. fileCount => 4
  character(len=MAXLEN), parameter :: genomeDstrbFilename = &
      "genomeDstrb_f08.csv"
  character(len=MAXLEN), parameter :: genomeDstrbFormat = "(*(f10.5, ','))"
  character(len=MAXLEN), private, parameter :: genomeDstrbPosition = "asis"
  integer, parameter :: genomeDstrbUnit = 103

  ! Death record. fileCount => 5
  character(len=MAXLEN), parameter :: deathFilename = "death_f08.csv"
  character(len=MAXLEN), parameter :: deathFormat = "(i6)"
  character(len=MAXLEN), parameter :: deathPosition = "asis"
  integer, parameter :: deathUnit = 104
  ! -------------------------------------------------------------------------- !
  ! Filename array
  character(len=MAXLEN), parameter :: filenames(FILECOUNT) = &
      [popFilename, timeFilename, ageDstrbFilename, genomeDstrbFilename, &
       deathFilename]
  ! Format array
  character(len=MAXLEN), parameter :: formats(FILECOUNT) = &
    [popFormat, timeFormat, ageDstrbFormat, genomeDstrbFormat, deathFormat]
  ! Position array
  character(len=MAXLEN), parameter :: positions(FILECOUNT) = &
    [popPosition, timePosition, ageDstrbPosition, genomeDstrbPosition, &
     deathPosition]
  ! Unit array
  integer, parameter :: units(FILECOUNT) = &
      [popUnit, timeUnit, ageDstrbUnit, genomeDstrbUnit, deathUnit]
  ! -------------------------------------------------------------------------- !
  
  ! File flags
  integer, public, parameter :: nullFlag = 0
  integer, public, parameter :: popFlag = 1
  integer, public, parameter :: timeFlag = 2
  integer, public, parameter :: ageDstrbFlag = 3
  integer, public, parameter :: genomeDstrbFlag = 4
  integer, public, parameter :: deathFlag = 5

  !----------------------------------------------------------------------------!
  ! GENERIC SUBROUTINE: constructWriter
  !>  Construct a `Writer` object.
  !----------------------------------------------------------------------------!
  interface constructWriter
    procedure :: constructWriter_array
    procedure :: constructWriter_scalar
  end interface constructWriter

  public :: constructWriter
contains

  ! === `Writer` CONSTRUCTOR SPECIFIC PROCEDURES ===
  function constructWriter_array(flags, initialize) result(new)
    implicit none
    integer, intent(in) :: flags(:)
    logical, optional   :: initialize

    type(Writer) :: new
    integer      :: i
    integer      :: flag

    allocate(new%enabledFlags(0))
    allocate(new%liveFlags(0))

    do i = 1, size(flags)
      flag = flags(i)
      if (flag <= 0 .or. flag > fileCount) then
        print "(a, i3)", "Invalid flag! flag: ", flag
      else
        call arrayInsert(new%enabledFlags, 1, flag)
      end if
    end do

    if (present(initialize)) then
      if (initialize) then
        call new%initialize
      end if
    end if
  end function constructWriter_array


  function constructWriter_scalar(flag, initialize) result(new)
    implicit none
    integer, intent(in) :: flag
    logical, optional   :: initialize

    type(Writer) :: new

    allocate(new%enabledFlags(0))
    allocate(new%liveFlags(0))

    if (flag <= 0 .or. flag > fileCount) then
      print "(a, i3)", "Invalid flag! flag: ", flag
      return
    else
      call arrayInsert(new%enabledFlags, 1, flag)
    end if

    if (present(initialize)) then
      if (initialize) then
        call new%initialize
      end if
    end if
  end function constructWriter_scalar


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]initialize
  !>  Initialize the files specified to be written in. The files are
  !!  specified by the integer `flag`. Multiple `flag`s can also be
  !!  as an automatic array of integers.
  !----------------------------------------------------------------------------!
  ! === INITIALIZE `Writer` SPECIFIC PROCEDURES === 
  subroutine initializeWriter(filename, unit, position)
    implicit none

    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: position
    integer, intent(in)          :: unit
  
    logical :: exists

    inquire(file=filename, exist=exists)

    if (exists) then
      open(unit, file=filename, status="old", position=position)
    else
      open(unit, file=filename, status="new")
    end if
  end subroutine initializeWriter


  subroutine writer_initializeAll(self)
    implicit none
    class(Writer), intent(inout) :: self

    integer :: flag
    integer :: i

    if (size(self%enabledFlags) == 0) return

    ! Put all enabled flags into live flags
    if(allocated(self%liveFlags)) deallocate(self%liveFlags)
    allocate(self%liveFlags(size(self%enabledFlags)))

    self%liveFlags = self%enabledFlags  ! Hopefully, just a copy.

    do i = 1, size(self%enabledFlags)
      flag = self%enabledFlags(i)
      call initializeWriter(filenames(flag), units(flag), positions(flag))
    end do
  end subroutine writer_initializeAll


  subroutine writer_initialize(self, flag)
    implicit none

    class(Writer), intent(inout) :: self
    integer, intent(in)          :: flag

    if (.not.any(self%enabledFlags == flag)) return

    ! Put enabled flag to live flag
    if(allocated(self%liveFlags)) deallocate(self%liveFlags)
    allocate(self%liveFlags(1))

    self%liveFlags = [flag]  ! NOTE: Automatic allocation

    call initializeWriter(filenames(flag), units(flag), positions(flag))
  end subroutine writer_initialize


  subroutine writer_listInitialize(self, flags)
    implicit none

    class(Writer), intent(inout) :: self
    integer, intent(in)          :: flags(:)

    integer :: i
    integer :: flag

    if(allocated(self%liveFlags)) deallocate(self%liveFlags)
    allocate(self%liveFlags(0))

    do i = 1, size(flags)
      flag = flags(i)
      if (.not.any(self%enabledFlags == flag)) cycle
      call arrayInsert(self%liveFlags, 1, flag)
      call initializeWriter(filenames(flag), units(flag), positions(flag))
    end do
  end subroutine writer_listInitialize


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]write
  !>  Write `arg` into the file specified by `flag`. The procedure
  !!  accepts real or integer arguments of either rank 0 or 1.
  !----------------------------------------------------------------------------!
  ! === WRITER WRITE ===
  subroutine writer_write_int(self, flag, arg)
    implicit none

    class(Writer), intent(inout)            :: self
    integer(kind=writeIntKind), intent(in)  :: arg
    integer, intent(in)                     :: flag

    if (.not.any(self%liveFlags == flag)) return
    write(units(flag), formats(flag)) arg
  end subroutine writer_write_int


  subroutine writer_write_real(self, flag, arg)
    implicit none

    class(Writer), intent(inout)         :: self
    integer, intent(in)                  :: flag
    real(kind=writeRealKind), intent(in) :: arg

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_real


  subroutine writer_write_intArray(self, flag, arg)
    implicit none

    class(Writer), intent(inout)           :: self
    integer, intent(in)                    :: flag
    integer(kind=writeIntKind), intent(in) :: arg(:)

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_intArray


  subroutine writer_write_realArray(self, flag, arg)
    implicit none

    class(Writer), intent(inout)         :: self
    integer, intent(in)                  :: flag
    real(kind=writeRealKind), intent(in) :: arg(:)

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_realArray


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]close
  !>  Close a unit for writing files specified by the integer `flag`. 
  !!  To close multiple units, a rank-1 array of integers `flags` can
  !!  be passed. To close all units, pass nothing. 
  !----------------------------------------------------------------------------!
  ! === WRITER CLOSE SPECIFIC PROCEDURES=== 
  subroutine writer_closeAll(self)
    implicit none
    class(Writer), intent(inout) :: self

    integer :: flag
    integer :: i

    do i = 1, size(self%liveFlags)
      flag = self%liveFlags(i)
      close(units(flag))
    end do

    if (allocated(self%liveFlags)) then
      deallocate(self%liveFlags)
      allocate(self%liveFlags(0))
    end if
  end subroutine writer_closeAll


  subroutine writer_close(self, flag)
    implicit none

    class(Writer), intent(inout) :: self
    integer, intent(in)          :: flag

    if (.not.any(self%liveFlags == flag)) then
      print "(a, i2)", "Chosen flag is not initialized! flag: ", flag
      return
    end if

    close(units(flag))
    call arrayRemoveElem(self%enabledFlags, flag)
  end subroutine writer_close


  subroutine writer_listclose(self, flags)
    implicit none

    class(Writer), intent(inout) :: self
    integer, intent(in)          :: flags(:)

    integer :: i

    do i = 1, size(flags)
      if (.not.any(self%liveFlags == flags(i))) then
        print "(a, i2)", "Chosen flag is not initialized! flag: ", flags(i)
      else
        close(units(flags(i)))
        call arrayRemoveElem(self%enabledFlags, flags(i))
      end if
    end do
  end subroutine writer_listclose


  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]destructor
  !>  Deallocate the allocatable attributes `enabledFlags`
  !!  and `liveFlags`.
  !----------------------------------------------------------------------------!
  subroutine destructor(self)
    implicit none
    type(Writer), intent(inout) :: self

    if (allocated(self%enabledFlags)) deallocate(self%enabledFlags)
    if (allocated(self%liveFlags)) deallocate(self%liveFlags)
  end subroutine destructor
end module SaveFormat

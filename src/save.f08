module SaveFormat
  ! -------------------------------------------------------------------------- !
  ! MODULE:  SaveFormat
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for saving data
  ! -------------------------------------------------------------------------- !
  use UpdateArray
  use iso_fortran_env, only: int64, real64
  implicit none
  private

  ! Module integer and real kinds
  ! Note: Can be changed when this module is to be reused in other projects. 
  integer, public, parameter :: writeIK = int64
  integer, public, parameter :: writeRK = real64

  ! -------------------------------------------------------------------------- !
  ! `Writer` derived type. A reusable unified interface for writing files.
  type, public :: Writer
    private
    integer, public, allocatable :: enabledFlags(:)
    integer,         allocatable :: liveFlags(:)
  contains
    private
    generic, public :: initialize => &
        writer_initialize, &
        writer_initializeAll, &
        writer_listInitialize
    generic, public :: close => &
        writer_close, &
        writer_closeAll, &
        writer_listclose
    generic, public :: write => &
        writer_write_int, &
        writer_write_real, &
        writer_write_intArray, &
        writer_write_realArray
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
  character(len=MAXLEN), parameter :: timeFormat = "(*(f10.5, ','))"
  character(len=MAXLEN), parameter :: timePosition = "append"
  integer, parameter :: timeUnit = 101

  ! Age demographics record. fileCount => 3
  character(len=MAXLEN), parameter :: ageDstrbFilename = "ageDstrb_f08.csv"
  character(len=MAXLEN), parameter :: ageDstrbFormat = "(*(i6, ','))"
  character(len=MAXLEN), parameter :: ageDstrbPosition = "asis"
  integer, parameter :: ageDstrbUnit = 102

  ! Genome demographics record. fileCount => 4
  character(len=MAXLEN), parameter :: genomeDstrbFilename = &
      "genomeDstrb_f08.csv"
  character(len=MAXLEN), parameter :: genomeDstrbFormat = "(*(i6, ','))"
  character(len=MAXLEN), private, parameter :: genomeDstrbPosition = "asis"
  integer, parameter :: genomeDstrbUnit = 103

  ! Death record. fileCount => 5
  ! Format: <death by old age> <death by mutation> <death by Verhulst factor>
  character(len=MAXLEN), parameter :: deathFilename = "death_f08.csv"
  character(len=MAXLEN), parameter :: deathFormat = "(*(i6, ','))"
  character(len=MAXLEN), parameter :: deathPosition = "asis"
  integer, parameter :: deathUnit = 104
  ! -------------------------------------------------------------------------- !
  ! Unit array
  integer, public, parameter :: units(FILECOUNT) = &
      [popUnit, timeUnit, ageDstrbUnit, genomeDstrbUnit, deathUnit]
  
  ! Filename array
  character(len=MAXLEN), public, parameter :: filenames(FILECOUNT) = &
      [popFilename, timeFilename, ageDstrbFilename, genomeDstrbFilename, &
       deathFilename]
  
  ! Position array
  character(len=MAXLEN), public, parameter :: positions(FILECOUNT) = &
    [popPosition, timePosition, ageDstrbPosition, genomeDstrbPosition, &
     deathPosition]
  ! -------------------------------------------------------------------------- !
  
  ! File flags
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

  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]write
  !>  Write `arg` into the file specified by `flag`. The procedure
  !!  accepts real or integer arguments of either rank 0 or 1.
  !----------------------------------------------------------------------------!
  interface
    module subroutine writer_write_int(self, flag, arg)
      class(Writer),         intent(inout) :: self
      integer(kind=writeIK), intent(in)    :: arg
      integer,               intent(in)    :: flag
    end subroutine

    module subroutine writer_write_real(self, flag, arg)
      class(Writer),       intent(inout) :: self
      real(kind=writeRK), intent(in)    :: arg
      integer,             intent(in)    :: flag
    end subroutine

    module subroutine writer_write_intArray(self, flag, arg)
      class(Writer),         intent(inout) :: self
      integer(kind=writeIK), intent(in)    :: arg(:)
      integer,               intent(in)    :: flag
    end subroutine

    module subroutine writer_write_realArray(self, flag, arg)
      class(Writer),       intent(inout) :: self
      real(kind=writeRK), intent(in)    :: arg(:)
      integer,             intent(in)    :: flag
    end subroutine
  end interface

  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]initialize
  !>  Initialize the files specified to be written in. The files are
  !!  specified by the integer `flag`. Multiple `flag`s can also be
  !!  as an automatic array of integers.
  !----------------------------------------------------------------------------!
  interface
    module subroutine initializeWriter(filename, unit, position)
      character(len=*), intent(in) :: filename
      character(len=*), intent(in) :: position
      integer,          intent(in) :: unit
    end subroutine

    module subroutine writer_initializeAll(self)
      class(Writer), intent(inout) :: self
    end subroutine

    module subroutine writer_initialize(self, flag)
      class(Writer), intent(inout) :: self
      integer,       intent(in)    :: flag
    end subroutine


    module subroutine writer_listInitialize(self, flags)
      class(Writer), intent(inout) :: self
      integer,       intent(in)    :: flags(:)
    end subroutine
  end interface

  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]close
  !>  Close a unit for writing files specified by the integer `flag`. 
  !!  To close multiple units, a rank-1 array of integers `flags` can
  !!  be passed. To close all units, pass nothing. 
  !----------------------------------------------------------------------------!
  interface
    module subroutine writer_closeAll(self)
      class(Writer), intent(inout) :: self
    end subroutine

    module subroutine writer_close(self, flag)
      class(Writer), intent(inout) :: self
      integer,       intent(in)    :: flag
    end subroutine

    module subroutine writer_listclose(self, flags)
      class(Writer), intent(inout) :: self
      integer,       intent(in)    :: flags(:)
    end subroutine
  end interface

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



submodule (SaveFormat) writer_initialize_procedures
  !----------------------------------------------------------------------------!
  ! SUBMODULE: writer_initialize_procedures
  !>  Submodule containing the specific procedures for the generic
  !!  type-bound procedure `[Writer]%initialize`.
  !----------------------------------------------------------------------------!
  implicit none
contains
  module subroutine initializeWriter(filename, unit, position)
    implicit none

    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: position
    integer,          intent(in) :: unit
  
    logical :: exists

    inquire(file=filename, exist=exists)

    if (exists) then
      open(unit, file=filename, status="old", position=position)
    else
      open(unit, file=filename, status="new")
    end if
  end subroutine initializeWriter


  module subroutine writer_initializeAll(self)
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


  module subroutine writer_initialize(self, flag)
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


  module subroutine writer_listInitialize(self, flags)
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
end submodule writer_initialize_procedures



submodule (SaveFormat) writer_write_procedures
  !----------------------------------------------------------------------------!
  ! SUBMODULE: writer_write_procedures
  !>  Submodule containing the specific procedures for the generic
  !!  type-bound procedure `[Writer]%write`.
  !----------------------------------------------------------------------------!
  implicit none
  ! Format array
  character(len=MAXLEN), parameter :: formats(FILECOUNT) = &
      [popFormat, timeFormat, ageDstrbFormat, genomeDstrbFormat, deathFormat]
  
contains
  module subroutine writer_write_int(self, flag, arg)
    implicit none
    

    class(Writer), intent(inout)            :: self
    integer(kind=writeIK), intent(in)  :: arg
    integer, intent(in)                     :: flag

    if (.not.any(self%liveFlags == flag)) return
    write(units(flag), formats(flag)) arg
  end subroutine writer_write_int


  module subroutine writer_write_real(self, flag, arg)
    implicit none

    class(Writer), intent(inout)         :: self
    integer, intent(in)                  :: flag
    real(kind=writeRK), intent(in) :: arg

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_real


  module subroutine writer_write_intArray(self, flag, arg)
    implicit none

    class(Writer), intent(inout)           :: self
    integer, intent(in)                    :: flag
    integer(kind=writeIK), intent(in) :: arg(:)

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_intArray


  module subroutine writer_write_realArray(self, flag, arg)
    implicit none

    class(Writer), intent(inout)         :: self
    integer, intent(in)                  :: flag
    real(kind=writeRK), intent(in) :: arg(:)

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_realArray
end submodule writer_write_procedures



submodule (SaveFormat) writer_close_procedures
  !----------------------------------------------------------------------------!
  ! SUBMODULE: writer_close_procedures
  !>  Submodule containing the specific procedures for the generic
  !!  type-bound procedure `[Writer]%close`.
  !----------------------------------------------------------------------------!
  implicit none
contains
  module subroutine writer_closeAll(self)
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


  module subroutine writer_close(self, flag)
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


  module subroutine writer_listclose(self, flags)
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
end submodule

module WriterType
  ! -------------------------------------------------------------------------- !
  ! MODULE:  WriterType
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing the `Writer` type, a type (class) for creating
  !!  unified interface for writing files.
  ! -------------------------------------------------------------------------- !
  use UpdateArray
  use SaveFormat
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
    procedure, public :: writeHeader => writer_writeHeader
    final :: destroy

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

  ! File flags
  public :: popFlag
  public :: timeFlag
  public :: ageDstrbFlag
  public :: deathFlag
  public :: divIdxFlag
    !! Shannon diversity index per time step.
  public :: timeFlag
    !! Timing statistics.

  public :: recordFlagArray
    !! Array of format flags.

  !----------------------------------------------------------------------------!
  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer % ]write
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
  ! BOUND SUBROUTINE: [Writer % ]initialize
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
  ! BOUND SUBROUTINE: [Writer % ]close
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

  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer % ]writeHeader
  !>  Write the header of the .csv file to write on.
  !----------------------------------------------------------------------------!
  interface
    module subroutine writer_writeHeader(self, flag, header)
      class(Writer),    intent(in) :: self
      integer,          intent(in) :: flag
      character(len=*), intent(in) :: header(:)
    end subroutine
  end interface


  interface
    module subroutine constructWriter_array(new, flags, initialize)
      type(Writer),      intent(out) :: new
      integer,           intent(in)  :: flags(:)
      logical, optional, intent(in)  :: initialize
    end subroutine

    module subroutine constructWriter_scalar(new, flag, initialize)
      type(Writer),      intent(out) :: new
      integer,           intent(in)  :: flag
      logical, optional, intent(in)  :: initialize
    end subroutine

    module subroutine destroy(self)
      type(Writer), intent(inout) :: self
    end subroutine destroy
  end interface

  !----------------------------------------------------------------------------!
  ! GENERIC SUBROUTINE: constructWriter
  !>  Construct a `Writer` object.
  !----------------------------------------------------------------------------!
  interface constructWriter
    procedure :: constructWriter_array
    procedure :: constructWriter_scalar
  end interface constructWriter
  !----------------------------------------------------------------------------!

  public :: constructWriter
end module WriterType

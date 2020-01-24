module WriterType
  ! -------------------------------------------------------------------------- !
  ! MODULE:  WriterType
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing the `Writer` type, a type (class) for creating
  !!  unified interface for writing files.
  ! -------------------------------------------------------------------------- !
  use, intrinsic :: iso_fortran_env, only: writeIK => int64, &
    writeRK => real64
  implicit none
  private

  ! Note: Can be changed when this module is to be reused in other projects. 
  public :: writeIK
    !! Integer kind for integer arguments of procedures in this module.
  public :: writeRK
    !! Real kind for integer arguments of procedures in this module.

  integer, parameter, public :: MAX_LEN = 32
    !! Max character length.

  ! -------------------------------------------------------------------------- !
  type, public :: OutputFile
    !! Output file structure. A convenient container of classes.
    character(MAX_LEN) :: filename
    character(MAX_LEN) :: format
    character(MAX_LEN) :: position
    integer :: unit
    integer :: flag
  end type

  ! All defined `OutputFile` objects.
  type(OutputFile), allocatable, protected, public :: outputFiles(:)

  ! -------------------------------------------------------------------------- !
  ! `Writer` derived type. A reusable unified interface for writing files.
  type, public :: Writer
    private
    type(OutputFile), allocatable :: availableFiles(:)
    type(OutputFile), allocatable :: liveFiles(:)
  contains
    private
    generic, public :: initialize => &
        writer_initialize, &
        writer_initializeAll, &
        writer_listInitialize
    !! Initialize writing of data as specified by the integer flag(s).
    !! Passing an integer initializes the corresponding output file.
    !! Passing an array of integer initializes all the output files specified
    !! by the elements of the array.
    !! Passing none initializes all defined output files in `SaveFormat` module.

    generic, public :: close => &
        writer_close, &
        writer_closeAll, &
        writer_listclose
    !! Close specified active output files as specified by integer flag(s).
    !! Passing an integer closes the corresponding output file.
    !! Passing an array of integers closes all the corresponding output files.
    !! Passing none closes all active output files.

    generic, public :: write => &
        writer_write_int, &
        writer_write_real, &
        writer_write_intArray, &
        writer_write_realArray
    !! Write an integer or real value of rank 0 (scalar) or rank 1 (1D array).

    procedure, public :: writeHeader => writer_writeHeader
    !! Write the header of the CSV file.
    ! final :: destroy
    !! Destructor. Deallocate any allocated attributes.

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
    module subroutine constructWriter_array(new, files, initialize)
      type(Writer),      intent(inout) :: new
      type(OutputFile),  intent(in)    :: files(:)
      logical, optional, intent(in)    :: initialize
    end subroutine

    module subroutine constructWriter_scalar(new, file, initialize)
      type(Writer),      intent(inout) :: new
      type(OutputFile),  intent(in)    :: file
      logical, optional, intent(in)    :: initialize
    end subroutine

    module subroutine findFileByFlag(array, flag, foundFile)
      type(OutputFile), allocatable, intent(in)    :: array(:)
      type(OutputFile), allocatable, intent(inout) :: foundFile
      integer, intent(in) :: flag
    end subroutine

    module subroutine removeFilebyFlag(array, flag)
      type(OutputFile), allocatable, intent(inout) :: array(:)
      integer,                       intent(in)    :: flag
    end subroutine

    module subroutine appendOutputFile(array, file)
      type(OutputFile), allocatable, intent(inout) :: array(:)
      type(OutputFile),              intent(in)    :: file
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

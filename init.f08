module Model
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Model
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing the Penna model parameters.
  !
  !   Parameters
  !   ----------
  !     L : integer
  !         Length of genome of each individual.
  !     T : integer
  !         Threshold for number of mutations.
  !     B : integer
  !         Number of births an individual can give.
  !     M : integer
  !         Number of mutations an individual will incur.
  !     R : integer
  !         The age at which individuals can reproduce.
  !     R_max : integer
  !         The inclusive upper limit age an individual can reproduce.
  !     N_start : integer
  !         Starting population count.
  !     K : integer
  !         Carrying capacity.
  !     Verhulst weight : array[real]
  !         Weights of the Verhulst factor per age. 
  !         Modified Verhulst factors become as so.
  !                         v_i = 1 - (N(t)/K)*w_i
  ! 
  !         where v_k  : Verhulst factor at age `i`.
  !               N(t) : Population size at time `t`.
  !               K    : The carrying capacity.
  !               w_k  : Verhulst weight at age `i`.
  !
  !  NOTE: Parameters with `_D` suffixes are default values
  ! -------------------------------------------------------------------------- !
  use iso_fortran_env, only: real64, int64
  implicit none

  integer, save :: MODEL_L = 32              ! Genome length (unmodifiable)
  integer, save :: MODEL_T = 3               ! Mutation threshold
  integer, save :: MODEL_B = 1               ! Birth rate
  integer, save :: MODEL_M = 1               ! Mutation rate
  integer, save :: MODEL_R = 9               ! Reproduction age
  integer, save :: MODEL_R_MAX = 9           ! Maximum reproduction age
  integer, save :: MODEL_K = 20000           ! Carrying capacity
  integer, save :: MODEL_N0_D = 100          ! Default starting pop size
  integer, save :: MODEL_TIME_STEPS_D = 100  ! Default total time steps
  real, allocatable, save :: MODEL_VERHULST_W(:)  ! Verhulst weights
  ! -------------------------------------------------------------------------- !
  ! Standard integer and real kinds.
  ! NOTE: This does not apply for counter, timing and throwaway variables.
  !       This is solely for variables related to gene and genomes.
  integer, parameter :: stdIntKind = int64
  integer, parameter :: stdRealKind = real64
  ! -------------------------------------------------------------------------- !

  integer, private, parameter :: modelParamCount = 7
  integer, private, parameter :: MAXLEN = 32
  integer, private, parameter :: nullValue = -1
  character(len=MAXLEN), private, parameter :: extParamName(modelParamCount) = &
      ["L", "T", "B", "M", "R", "S", "K"]
  character(len=MAXLEN), private, parameter :: endOfList = "//"

  character(len=MAXLEN) :: modelFilename = "model.ini"
  character(len=MAXLEN) :: vWeightsFilename = "verhulst_weights.ini"
  integer, private, parameter :: modelUnit = 99
  integer, private, parameter :: vWeightUnit = 98

  private :: assignParameters
  private :: getCharArrayIndex
contains

  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readIni
  !>  Read the model parameters from a .ini file.
  ! -------------------------------------------------------------------------- !
  subroutine readIni
    implicit none
    integer :: status
    integer :: readStatus
    integer :: i
    integer :: values(modelParamCount)
    integer :: tempValue
    character(len=MAXLEN) :: key

    ! Initialize `value`
    values(:) = nullValue

    ! Check whether the file exists or not.
    inquire(file=modelFilename, iostat=status)
    if (status /= 0) then
      print "(3a)", "***Cannot read '", modelFilename, &
          "'. Using the default values."
      return
    end if

    ! Read file
    open(unit=modelUnit, file=modelFilename)
    do i = 1, modelParamCount
      read(modelUnit, "(a2, i6)", iostat=readStatus) key, tempValue

      ! Case handling
      if (readStatus /= 0) then
        print "(a, i2)", "***Cannot read line. Ending at line ", i
        exit
      else if(key == endOfList) then
        print *, "***Reading ended prematurely"
        return
      else if(.not.any(extParamName == key)) then
        print "(a, a1, a)", "***Warning. '", key, &
            "' is not a valid parameter."
        cycle
      end if
      values(getCharArrayIndex(extParamName, key)) = tempValue
    end do

    call assignParameters(values)
    close(modelUnit)
  end subroutine readIni

  
  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readVerhulstWeights
  !>  Read the Verhulst weights values from a .ini file.
  ! -------------------------------------------------------------------------- !
  subroutine readVerhulstWeights
    implicit none
    ! TODO
    if (.not.allocated(MODEL_VERHULST_W)) allocate(MODEL_VERHULST_W(MODEL_L))
    MODEL_VERHULST_W(:) = 0.
  end subroutine readVerhulstWeights


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocVerhulstWeights
  !>  Deallocate `MODEL_VERHULST_W` array.
  ! -------------------------------------------------------------------------- !
  subroutine deallocVerhulstWeights
    implicit none
    if (allocated(MODEL_VERHULST_W)) deallocate(MODEL_VERHULST_W)
  end subroutine deallocVerhulstWeights


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignParameters
  !>  Assign model parameters from an array of integer `values`.
  ! -------------------------------------------------------------------------- !
  subroutine assignParameters(values)
    implicit none
    integer, intent(in) :: values(:)
    integer :: i

    ! NOTE: I can't think of a more elegant solution to this.
    do i = 1, modelParamCount
      select case(i)
      case(1)
        MODEL_T = values(i)
      case(2)
        MODEL_T = values(i)
      case(3)
        MODEL_B = values(i)
      case(4)
        MODEL_M = values(i)
      case(5)
        MODEL_R = values(i)
      case(6)
        MODEL_R_MAX = values(i)
      case(7)
        MODEL_K = values(i)
      end select
    end do
  end subroutine assignParameters


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getCharArrayIndex
  !>  Get the corresponding index of `elem` in a rank-1 array of characters
  !   `elem`. If `elem` is not found in `array`, it returns `nullValue` which
  !   is set to 0.
  ! -------------------------------------------------------------------------- !
  function getCharArrayIndex(array, elem) result(i)
    implicit none
    character(len=MAXLEN), intent(in) :: array(:)
    character(len=MAXLEN), intent(in) :: elem

    integer :: i

    do i = 1, size(array)
      if (array(i) == elem) return
    end do
 
    i = nullValue  ! Default value
  end function getCharArrayIndex
end module Model


module Flag
  ! -------------------------------------------------------------------------- !
  ! MODULE:  Flag
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION: 
  !>  Module containing `flags` for indicating an individual's state.
  ! -------------------------------------------------------------------------- !
  integer, parameter :: ALIVE = 1
  integer, parameter :: DEAD_OLD_AGE = 2
  integer, parameter :: DEAD_MUTATION = 3
  integer, parameter :: DEAD_VERHULST = 4
  integer, parameter :: DEATH_REASONS(4) = &
      [ALIVE,        &
      DEAD_OLD_AGE,  &
      DEAD_MUTATION, &
      DEAD_VERHULST]
end module Flag


module SaveFormat
  ! -------------------------------------------------------------------------- !
  ! MODULE:  SaveFormat
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing formats of data files.
  ! -------------------------------------------------------------------------- !
  use UpdateArray, only: arrayInsert, arrayRemoveElem
  use iso_fortran_env, only: real64, int64
  implicit none
  private

  ! `Writer` derived type. A unified interface for writing files.
  type, public :: Writer
    private
    integer, public, allocatable :: enabledFlags(:)
    integer, allocatable         :: liveFlags(:)
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

  ! Number of Files to save
  integer, public, parameter :: FILECOUNT = 5
  
  ! Max character length
  integer, parameter :: MAXLEN = 32
  ! Default kinds
  integer, parameter :: writeRealKind = real64
  integer, parameter :: writeIntKind = int64

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

  ! File flags
  integer, public, parameter :: nullFlag = 0
  integer, public, parameter :: popFlag = 1
  integer, public, parameter :: timeFlag = 2
  integer, public, parameter :: ageDstrbFlag = 3
  integer, public, parameter :: genomeDstrbFlag = 4
  integer, public, parameter :: deathFlag = 5

  !----------------------------------------------------------------------------!
  ! GENERIC SUBROUTINE: constructWriter
  !>  Construct a `Writer` type.
  !----------------------------------------------------------------------------!
  interface constructWriter
    procedure :: constructWriter_array
    procedure :: constructWriter_scalar
  end interface constructWriter
  public :: constructWriter
contains

  ! === `Writer` CONSTRUCTOR PROCEDURES ===
  function constructWriter_array(flags, initialize) result(new)
    integer, intent(in) :: flags(:)
    logical, optional   :: initialize

    type(Writer) :: new
    integer :: i
    integer :: flag

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
  !>  Initialize the files specified to be written on. The files are specified
  !   by the integer `flag`. Optionally, multiple `flag`s can be passed as an
  !   automatic array of integers.
  !----------------------------------------------------------------------------!
  ! === INITIALIZE `Writer` SPECIFIC PROCEDURES === 
  subroutine initializeWriter(filename, unit, position)
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
  !>  Write `arg` into the file specified by `flag`. The procedure can accept
  !   real and integer arguments of rank 0 or 1.
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
    real(kind=writeRealKind), intent(in) :: arg
    integer, intent(in)                  :: flag

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_real


  subroutine writer_write_intArray(self, flag, arg)
    implicit none
    class(Writer), intent(inout)           :: self
    integer(kind=writeIntKind), intent(in) :: arg(:)
    integer, intent(in)                    :: flag

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_intArray


  subroutine writer_write_realArray(self, flag, arg)
    implicit none
    class(Writer), intent(inout)         :: self
    real(kind=writeRealKind), intent(in) :: arg(:)
    integer, intent(in)                  :: flag

    if (.not.any(self%liveFlags == flag)) return

    write(units(flag), formats(flag)) arg
  end subroutine writer_write_realArray

  !----------------------------------------------------------------------------!
  ! BOUND SUBROUTINE: [Writer%]close
  !>  Close a unit for writing files specified by the integer `flag`. To close
  !   multiple units, a rank-1 array of integers `flags` can be passed. To close
  !   all units, pass nothing. 
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
  !>  Deallocate the allocatable attributes `enabledFlags` and `liveFlags`.
  !----------------------------------------------------------------------------!
  subroutine destructor(self)
    implicit none
    type(Writer), intent(inout) :: self

    if (allocated(self%enabledFlags)) deallocate(self%enabledFlags)
    if (allocated(self%liveFlags)) deallocate(self%liveFlags)
  end subroutine destructor
end module SaveFormat


module WriterOptions
  ! -------------------------------------------------------------------------- !
  ! MODULE: WriterOptions
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing format of the text files to be saved. 
  ! -------------------------------------------------------------------------- !
  use WriterType
  implicit none
  private

  ! SAVED FILE FORMATS
  ! -------------------------------------------------------------------------- !
  ! Population size/count per time step record.
  integer, parameter :: popUnit = 100
  character(len=MAX_LEN), parameter :: popFilename = "pop_size_f08.csv"
  character(len=MAX_LEN), parameter :: popFormat = "(i6)"
  character(len=MAX_LEN), parameter :: popPosition = "asis"
  type(OutputFile) :: popFile

  ! Age demographics of the last 300 time steps record.
  integer, parameter :: ageDstrbUnit = 101
  character(len=MAX_LEN), parameter :: ageDstrbFilename = "ageDstrb_f08.csv"
  character(len=MAX_LEN), parameter :: ageDstrbFormat = "(*(i6, ','))"
  character(len=MAX_LEN), parameter :: ageDstrbPosition = "asis"
  type(OutputFile) :: ageDstrbFile
  
  ! Death counts (age, mutation, Verhulst (random) killiing) per time 
  ! step record.
  ! Format: <death by old age> <death by mutation> <death by Verhulst factor>
  integer, parameter :: deathUnit = 102
  character(len=MAX_LEN), parameter :: deathFilename = "death_f08.csv"
  character(len=MAX_LEN), parameter :: deathFormat = "(*(i6, ','))"
  character(len=MAX_LEN), parameter :: deathPosition = "asis"
  type(OutputFile) :: deathFile

  ! Shannon diversity index of genomes per time step record.
  integer, parameter :: divIdxUnit = 103
  character(len=MAX_LEN), parameter :: divIdxFilename = "divIdx_f08.csv"
  character(len=MAX_LEN), parameter :: divIdxFormat = "(f15.10)"
  character(len=MAX_LEN), parameter :: divIdxPosition = "asis"
  type(OutputFile) :: divIdxFile

  ! Timing statistics record.
  ! Format: <no. of time step> <no. of population> <wall time>
  integer, parameter :: timeUnit = 104
  character(len=MAX_LEN), parameter :: timeFilename = "time_f08.csv"
  character(len=MAX_LEN), parameter :: timeFormat = "(*(f10.5, ','))"
  character(len=MAX_LEN), parameter :: timePosition = "append"
  type(OutputFile) :: timeFile

  ! -------------------------------------------------------------------------- !
  ! Record flags. Corresponds to the data to be recorded.
  integer, public, parameter :: nullFlag = 0
    !! Nothing (do not record).
  integer, public, parameter :: popFlag = 1
    !! Population size per time step.
  integer, public, parameter :: ageDstrbFlag = 2
    !! Age distribution in the last 300 time steps
  integer, public, parameter :: deathFlag = 3
    !! Death counts (death by age, by mutation, by Verhulst factor) 
    !! per time step.
  integer, public, parameter :: divIdxFlag = 4
    !! Shannon diversity index per time step.
  integer, public, parameter :: timeFlag = 5
    !! Timing statistics.
  ! -------------------------------------------------------------------------- !

  ! Initialization procedures. 
  public :: initializeWriterObjects
  public :: constructAvailableWriter
  
  ! Public `WriterType` procedures and derived types. 
  public :: Writer
  public :: writeIK
  public :: writeRK
  public :: deallocWriterTypeAlloctbl
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeWriterObjects
  !>  Assign all defined `OutputFile` object in `WriterOptions` module.
  ! -------------------------------------------------------------------------- !
  subroutine initializeWriterObjects()
    ! Population size per time step file.
    popFile = OutputFile(popFilename, popFormat, popPosition, popUnit, popFlag)
    ! Final age distribution file.
    ageDstrbFile = OutputFile(ageDstrbFilename, ageDstrbFormat, &
        ageDstrbPosition, ageDstrbUnit, ageDstrbFlag)
    ! Death counts per time step file.
    deathFile = OutputFile(deathFilename, deathFormat, deathPosition, &
        deathUnit, deathFlag)
    ! Diversity index per time step file.
    divIdxFile = OutputFile(divIdxFilename, divIdxFormat, divIdxPosition, &
        divIdxUnit, divIdxFlag)
    ! Time statistics file.
    timeFile = OutputFile(timeFilename, timeFormat, timePosition, timeUnit, &
        timeFlag)
    
    call declareAvailableFiles([popFile, ageDstrbFile, deathFile, divIdxFile, &
        timeFile])
  end subroutine initializeWriterObjects


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getOutputFile
  !>  Get the `OutputFile` object corresponding to the passed `flag` integer.
  !!  This object contains the necessary information about the output file.
  ! -------------------------------------------------------------------------- !
  function getOutputFile(flag) result(out)
    integer, intent(in) :: flag
      !! Flag to compare
    type(OutputFile)    :: out

    select case(flag)
      case (popFlag)
        out = popFile
      case (ageDstrbFlag)
        out = ageDstrbFile
      case (deathFlag)
        out = deathFile
      case (divIdxFlag)
        out = divIdxFile
      case (timeFlag)
        out = timeFile

      case default
        print "(a, i0, a)", "***ERROR. No file with flag (", flag, ") found."
        stop
    end select
  end function getOutputFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: constructAvailableWriter
  !>  Create and initialize if `initiliaze` is true a `Writer` object with the
  !!  defined `OutputFile` objects and their corresponding flags in
  !!  `WriterOptions` module.
  ! -------------------------------------------------------------------------- !
  subroutine constructAvailableWriter(out, flags, initialize)
    type(Writer),      intent(out) :: out
      !! New `Writer` object.
    integer,           intent(in)  :: flags(:)
      !! Record flags.
    logical, optional, intent(in)  :: initialize
      !! Initialize `new` immediately. Default is `.false.`
  
    type(OutputFile) :: foundFiles(size(flags))
    logical :: initialize_
    integer :: i

    do i = 1, size(flags)
      foundFiles(i) = getOutputFile(flags(i))
    end do

    ! Assign default value to 
    if (present(initialize)) then
      initialize_ = initialize
    else
      initialize_ = .false.
    end if

    call constructWriter(out, foundFiles, initialize_)
  end subroutine constructAvailableWriter
end module WriterOptions

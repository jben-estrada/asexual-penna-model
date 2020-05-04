module WriterOptions
  ! -------------------------------------------------------------------------- !
  ! MODULE: WriterOptions
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing format of the text files to be saved. 
  ! -------------------------------------------------------------------------- !
  use ErrorMSG, only: raiseError
  use Parameters, only: PROG_OUT_FILE_NAME
  use WriterType, only: &
    Writer,     &
    MAX_LEN,    &
    OutputFile, &
    writeIK,    &
    writeRK,    &
    freeWriterModAlloctbls, &
    constructWriter, &
    declareAvailableFiles
  implicit none
  private

  ! SAVED FILE FORMATS
  ! -------------------------------------------------------------------------- !
  ! Population size/count per time step record.
  integer, parameter :: popUnit = 100
  character(len=MAX_LEN), parameter :: popFormat = "(i6)"
  character(len=MAX_LEN), parameter :: popPosition = "asis"
  type(OutputFile) :: popFile

  ! Age demographics of the last 300 time steps record.
  integer, parameter :: ageDstrbUnit = 101
  character(len=MAX_LEN), parameter :: ageDstrbFormat = "(*(i6, ','))"
  character(len=MAX_LEN), parameter :: ageDstrbPosition = "asis"
  type(OutputFile) :: ageDstrbFile
  
  ! Death counts (age, mutation, Verhulst (random) killiing) per time 
  ! step record.
  ! Format: <death by old age> <death by mutation> <death by Verhulst factor>
  integer, parameter :: deathUnit = 102
  character(len=MAX_LEN), parameter :: deathFormat = "(*(i6, ','))"
  character(len=MAX_LEN), parameter :: deathPosition = "asis"
  type(OutputFile) :: deathFile

  ! Shannon diversity index of genomes per time step record.
  integer, parameter :: divIdxUnit = 103
  character(len=MAX_LEN), parameter :: divIdxFormat = "(f15.10)"
  character(len=MAX_LEN), parameter :: divIdxPosition = "asis"
  type(OutputFile) :: divIdxFile

  ! Bad gene distribution per time step.
  integer, parameter :: badGeneDstrbUnit = 104
  character(len=MAX_LEN), parameter :: badGeneDstrbFormat = "(*(i6, ','))"
  character(len=MAX_LEN), parameter :: badGeneDstrbPosition = "asis"
  type(OutputFile) :: badGeneDstrbFile

  ! Timing statistics record.
  ! Format: <no. of time step> <no. of population> <wall time>
  integer, parameter :: timeUnit = 105
  character(len=MAX_LEN), parameter :: timeFormat = "(*(f10.5, ','))"
  character(len=MAX_LEN), parameter :: timePosition = "append"
  type(OutputFile) :: timeFile

  ! -------------------------------------------------------------------------- !
  ! Record flags. Corresponds to the data to be recorded.
  character, public, parameter :: nullFlag = "x"
    !! Nothing (do not record).
  character, public, parameter :: popFlag = "p"
    !! Population size per time step.
  character, public, parameter :: ageDstrbFlag = "a"
    !! Age distribution in the last 300 time steps
  character, public, parameter :: deathFlag = "d"
    !! Death counts (death by age, by mutation, by Verhulst factor) 
    !! per time step.
  character, public, parameter :: divIdxFlag = "s"
    !! Shannon diversity index per time step.
  character, public, parameter :: badGeneFlag = "b"
    !! Bad gene distribution per time step.
  character, public, parameter :: timeFlag = "t"
    !! Timing statistics.

  character(len=*), parameter  :: FILE_NAME_TIME = "time_stat.csv"
    !! File name of time statisttics.
  ! -------------------------------------------------------------------------- !

  ! Initialization procedures. 
  public :: initWriterObjs
  public :: constructAvailableWriter
  
  ! Public `WriterType` procedures and derived types. 
  public :: Writer
  public :: writeIK
  public :: writeRK
  public :: freeWriterModAlloctbls
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initWriterObjs
  !>  Assign all defined `OutputFile` object in `WriterOptions` module.
  ! -------------------------------------------------------------------------- !
  subroutine initWriterObjs()
    ! Population size per time step file.
    popFile = OutputFile(PROG_OUT_FILE_NAME, popFormat, popPosition, popFlag, &
        popUnit)
    ! Final age distribution file.
    ageDstrbFile = OutputFile(PROG_OUT_FILE_NAME, ageDstrbFormat, &
        ageDstrbPosition, ageDstrbFlag, ageDstrbUnit)
    ! Death counts per time step file.
    deathFile = OutputFile(PROG_OUT_FILE_NAME, deathFormat, deathPosition, &
        deathFlag, deathUnit)
    ! Diversity index per time step file.
    divIdxFile = OutputFile(PROG_OUT_FILE_NAME, divIdxFormat, divIdxPosition, &
        divIdxFlag, divIdxUnit)
    ! Bad gene distribution per time step.
    badGeneDstrbFile = OutputFile(PROG_OUT_FILE_NAME, badGeneDstrbFormat, &
        badGeneDstrbPosition, badGeneFlag, badGeneDstrbUnit)
    ! Time statistics file.
    timeFile = OutputFile(FILE_NAME_TIME, timeFormat, timePosition, &
        timeFlag, timeUnit)
    
    call declareAvailableFiles([popFile, ageDstrbFile, deathFile, divIdxFile, &
        badGeneDstrbFile, timeFile])
  end subroutine initWriterObjs


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getOutputFile
  !>  Get the `OutputFile` object corresponding to the passed `flag` integer.
  !!  This object contains the necessary information about the output file.
  ! -------------------------------------------------------------------------- !
  function getOutputFile(flag) result(out)
    character, intent(in) :: flag
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
      case (badGeneFlag)
        out = badGeneDstrbFile
      case (timeFlag)
        out = timeFile

      case default
        call raiseError("No file with flag '" // flag // "' found.")
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
    character,         intent(in)  :: flags(:)
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
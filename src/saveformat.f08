module SaveFormat
  implicit none

  ! Number of Files to save
  integer, parameter :: FILECOUNT = 5
  
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
  character(len=MAXLEN), parameter :: genomeDstrbPosition = "asis"
  integer, parameter :: genomeDstrbUnit = 103

  ! Death record. fileCount => 5
  ! Format: <death by old age> <death by mutation> <death by Verhulst factor>
  character(len=MAXLEN), parameter :: deathFilename = "death_f08.csv"
  character(len=MAXLEN), parameter :: deathFormat = "(*(i6, ','))"
  character(len=MAXLEN), parameter :: deathPosition = "asis"
  integer, parameter :: deathUnit = 104

  ! -------------------------------------------------------------------------- !
  ! Unit array
  integer, parameter :: units(FILECOUNT) = &
      [popUnit, timeUnit, ageDstrbUnit, genomeDstrbUnit, deathUnit]
  
  ! Filename array
  character(len=MAXLEN), parameter :: filenames(FILECOUNT) = &
      [popFilename, timeFilename, ageDstrbFilename, genomeDstrbFilename, &
       deathFilename]
  
  ! Position array
  character(len=MAXLEN), parameter :: positions(FILECOUNT) = &
    [popPosition, timePosition, ageDstrbPosition, genomeDstrbPosition, &
     deathPosition]
  ! -------------------------------------------------------------------------- !
  
  ! File flags
  integer, parameter :: popFlag = 1
  integer, parameter :: timeFlag = 2
  integer, parameter :: ageDstrbFlag = 3
  integer, parameter :: genomeDstrbFlag = 4
  integer, parameter :: deathFlag = 5
end module SaveFormat
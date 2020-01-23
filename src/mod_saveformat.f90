module SaveFormat
  implicit none

  ! Number of Files to save
  integer, parameter :: FILECOUNT = 5
  
  integer, parameter :: MAX_LEN = 32
    !! Max character length.

  ! SAVED FILE FORMATS
  ! -------------------------------------------------------------------------- !
  ! Population size record. fileCount => 1
  character(len=MAXLEN), parameter :: popFilename = "pop_size_f08.csv"
  character(len=MAXLEN), parameter :: popFormat = "(i6)"
  character(len=MAXLEN), parameter :: popPosition = "asis"
  integer, parameter :: popUnit = 100

  ! Age demographics record. fileCount => 2
  character(len=MAXLEN), parameter :: ageDstrbFilename = "ageDstrb_f08.csv"
  character(len=MAXLEN), parameter :: ageDstrbFormat = "(*(i6, ','))"
  character(len=MAXLEN), parameter :: ageDstrbPosition = "asis"
  integer, parameter :: ageDstrbUnit = 101
  
  ! Death record. fileCount => 3
  ! Format: <death by old age> <death by mutation> <death by Verhulst factor>
  character(len=MAXLEN), parameter :: deathFilename = "death_f08.csv"
  character(len=MAXLEN), parameter :: deathFormat = "(*(i6, ','))"
  character(len=MAXLEN), parameter :: deathPosition = "asis"
  integer, parameter :: deathUnit = 102

  ! Genome demographics record. fileCount => 4
  character(len=MAXLEN), parameter :: divIdxFilename = "divIdx_f08.csv"
  character(len=MAXLEN), parameter :: divIdxFormat = "(f15.10)"
  character(len=MAXLEN), parameter :: divIdxPosition = "asis"
  integer, parameter :: divIdxUnit = 103

  ! Timing record. fileCount => 5
  ! Format: <no. of time step> <no. of population> <wall time>
  character(len=MAXLEN), parameter :: timeFilename = "time_f08.csv"
  character(len=MAXLEN), parameter :: timeFormat = "(*(f10.5, ','))"
  character(len=MAXLEN), parameter :: timePosition = "append"
  integer, parameter :: timeUnit = 104

  ! -------------------------------------------------------------------------- !
  ! File flags.
  integer, parameter :: nullFlag = 0
  integer, parameter :: popFlag = 1
  integer, parameter :: ageDstrbFlag = 2
  integer, parameter :: deathFlag = 3
  integer, parameter :: divIdxFlag = 4
  integer, parameter :: timeFlag = 5

  ! ARRAYS
  ! -------------------------------------------------------------------------- !
  ! Unit array.
  integer, parameter :: unitArray(FILECOUNT) = &
      [popUnit, &
       ageDstrbUnit, &
       deathUnit, &
       divIdxUnit, &
       timeUnit &
       ]
  
  ! Filename array.
  character(len=MAX_LEN), parameter :: filenameArray(FILECOUNT) = &
      [popFilename, &
       ageDstrbFilename, &
       deathFilename, &
       divIdxFilename, &
       timeFilename &
       ]
  
  ! Position array.
  character(len=MAX_LEN), parameter :: positionArray(FILECOUNT) = &
    [popPosition, &
     ageDstrbPosition, &
     deathPosition, &
     divIdxPosition, &
     timePosition &
     ]
    
  ! Format array.
  character(len=MAX_LEN), parameter :: formatArray(FILECOUNT) = &
     [popFormat, &
      ageDstrbFormat, &
      deathFormat, &
      divIdxFormat, &
      timeFormat &
      ]
  
  ! Format flag array. (NOTE: Different from record flags)
  integer, parameter :: recordFlagArray(FILECOUNT) = &
      [popFlag, &
       ageDstrbFlag, &
       deathFlag, &
       divIdxFlag, &
       timeFlag &
       ]
  ! -------------------------------------------------------------------------- !
end module SaveFormat
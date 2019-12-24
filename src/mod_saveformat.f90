module SaveFormat
  implicit none

  ! Number of Files to save
  integer, parameter :: FILECOUNT = 5
  
  ! Max character length
  integer, parameter :: MAXLEN = 32

  ! SAVED FILE FORMATS
  ! -------------------------------------------------------------------------- !
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
  
  ! Death record. fileCount => 4
  ! Format: <death by old age> <death by mutation> <death by Verhulst factor>
  character(len=MAXLEN), parameter :: deathFilename = "death_f08.csv"
  character(len=MAXLEN), parameter :: deathFormat = "(*(i6, ','))"
  character(len=MAXLEN), parameter :: deathPosition = "asis"
  integer, parameter :: deathUnit = 103

  ! Genome demographics record. fileCount => 5
  character(len=MAXLEN), parameter :: divIdxFilename = "divIdx_f08.csv"
  character(len=MAXLEN), parameter :: divIdxFormat = "(f15.10)"
  character(len=MAXLEN), parameter :: divIdxPosition = "asis"
  integer, parameter :: divIdxUnit = 104

  ! -------------------------------------------------------------------------- !
  ! File flags.
  integer, parameter :: popFlag = 1
  integer, parameter :: timeFlag = 2
  integer, parameter :: ageDstrbFlag = 3
  integer, parameter :: deathFlag = 4
  integer, parameter :: divIdxFlag = 5

  ! ARRAYS
  ! -------------------------------------------------------------------------- !
  ! Unit array.
  integer, parameter :: units(FILECOUNT) = &
      [popUnit, &
       timeUnit, &
       ageDstrbUnit, &
       deathUnit, &
       divIdxUnit &
       ]
  
  ! Filename array.
  character(len=MAXLEN), parameter :: filenames(FILECOUNT) = &
      [popFilename, &
       timeFilename, &
       ageDstrbFilename, &
       deathFilename, &
       divIdxFilename &
       ]
  
  ! Position array.
  character(len=MAXLEN), parameter :: positions(FILECOUNT) = &
    [popPosition, &
     timePosition, &
     ageDstrbPosition, &
     deathPosition, &
     divIdxPosition &
     ]
    
  ! Format array.
  character(len=MAXLEN), parameter :: formats(FILECOUNT) = &
     [popFormat, &
      timeFormat, &
      ageDstrbFormat, &
      deathFormat, &
      divIdxFormat &
      ]
  
  ! Format flag array. (NOTE: Different from record flags)
  integer, parameter :: formatFlags(FILECOUNT) = &
      [popFlag, &
       timeFlag, &
       ageDstrbFlag, &
       deathFlag, &
       divIdxFlag &
       ]
  ! -------------------------------------------------------------------------- !
end module SaveFormat
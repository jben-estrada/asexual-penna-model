module SaveFormat
  ! -------------------------------------------------------------------------- !
  ! MODULE: SaveFormat
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing format of the text files to be saved. 
  ! -------------------------------------------------------------------------- !
  implicit none

  integer, parameter :: FILECOUNT = 5
    !! Number of Files to save
  
  integer, parameter :: MAX_LEN = 32
    !! Max character length.

  ! SAVED FILE FORMATS
  ! -------------------------------------------------------------------------- !
  ! Population size record. fileCount => 1
  character(len=MAX_LEN), parameter :: popFilename = "pop_size_f08.csv"
  character(len=MAX_LEN), parameter :: popFormat = "(i6)"
  character(len=MAX_LEN), parameter :: popPosition = "asis"
  integer, parameter :: popUnit = 100

  ! Age demographics record. fileCount => 2
  character(len=MAX_LEN), parameter :: ageDstrbFilename = "ageDstrb_f08.csv"
  character(len=MAX_LEN), parameter :: ageDstrbFormat = "(*(i6, ','))"
  character(len=MAX_LEN), parameter :: ageDstrbPosition = "asis"
  integer, parameter :: ageDstrbUnit = 101
  
  ! Death record. fileCount => 3
  ! Format: <death by old age> <death by mutation> <death by Verhulst factor>
  character(len=MAX_LEN), parameter :: deathFilename = "death_f08.csv"
  character(len=MAX_LEN), parameter :: deathFormat = "(*(i6, ','))"
  character(len=MAX_LEN), parameter :: deathPosition = "asis"
  integer, parameter :: deathUnit = 102

  ! Genome demographics record. fileCount => 4
  character(len=MAX_LEN), parameter :: divIdxFilename = "divIdx_f08.csv"
  character(len=MAX_LEN), parameter :: divIdxFormat = "(f15.10)"
  character(len=MAX_LEN), parameter :: divIdxPosition = "asis"
  integer, parameter :: divIdxUnit = 103

  ! Timing record. fileCount => 5
  ! Format: <no. of time step> <no. of population> <wall time>
  character(len=MAX_LEN), parameter :: timeFilename = "time_f08.csv"
  character(len=MAX_LEN), parameter :: timeFormat = "(*(f10.5, ','))"
  character(len=MAX_LEN), parameter :: timePosition = "append"
  integer, parameter :: timeUnit = 104

  ! -------------------------------------------------------------------------- !
  ! Format/Record flags. Corresponds to the data to be recorded.
  integer, parameter :: nullFlag = 0
    !! Nothing (do not record).
  integer, parameter :: popFlag = 1
    !! Population size per time step.
  integer, parameter :: ageDstrbFlag = 2
    !! Age distribution in the last 300 time steps
  integer, parameter :: deathFlag = 3
    !! Death counts (death by age, by mutation, by Verhulst factor) 
    !! per time step.
  integer, parameter :: divIdxFlag = 4
    !! Shannon diversity index per time step.
  integer, parameter :: timeFlag = 5
    !! Timing statistics.

  ! CONVENIENT ARRAYS
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
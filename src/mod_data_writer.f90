module DataWriter
  ! -------------------------------------------------------------------------- !
  ! MODULE:  DataWriter
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for writing data onto files.
  ! -------------------------------------------------------------------------- !
  use Parameters, only: &
    MODEL_L,            &
    REC_NULL,           &
    REC_POP,            &
    REC_AGE_DSTRB,      &
    REC_DEATH,          &
    REC_DIV_IDX,        &
    REC_GENE_DSTRB,     &
    REC_GNM_COUNT,      &
    REC_TIME,           &
    REC_FLAG_PENNA,     &
    REC_FLAG_PROG,      &
    REC_FLAG_ORDER
  use CastProcs, only: castIntToChar
  use ErrorMSG, only: raiseError
  use WriterType, only: Writer, writeIK, writeRK
  implicit none
  private

  character(len=*), parameter   :: DIVIDER_READABLE = "---------------"
  character(len=:), allocatable :: divider
  logical :: dividerDelimSet = .false.

  ! Data file delimiters.
  character, parameter :: DELIM_READABLE = "|"
  character, parameter :: DELIM_CSV = ","
  character :: delim = char(0)

  ! Data writer types.
  integer, parameter :: DATA_WRITER_PENNA = 0
  integer, parameter :: DATA_WRITER_PROG  = 1

  ! Base IO unit.
  integer, parameter :: BASE_UNIT = 99

  ! All writer objects.
  type(Writer), target  :: writerArr(len(REC_FLAG_ORDER))
  logical :: initWriterArr(len(REC_FLAG_ORDER)) = .false.

  public :: initDataWriter
  public :: getWriterPtr
  public :: isWriterInitialized
  public :: closeDataWriter
  
  public :: DATA_WRITER_PENNA
  public :: DATA_WRITER_PROG

  public :: Writer
  public :: writeIK
  public :: writeRK
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getWriterPtr
  !>  Get a pointer to the writer object specified with a record flag.
  ! -------------------------------------------------------------------------- !
  function getWriterPtr(recordFlag) result(writerPtr)
    character, intent(in) :: recordFlag
    type(Writer), pointer :: writerPtr
    integer :: writerIdx

    writerIdx = scan(REC_FLAG_ORDER, recordFlag)
    if (writerIdx > 0) then
      writerPtr => writerArr(writerIdx)
    else
      writerPtr => null()
      if (recordFlag /= REC_NULL) then
        call raiseError("Invalid record flag: '" // recordFlag // "'")
      end if
    end if
  end function getWriterPtr


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initChosenWriter
  !>  Initialize a writer as specified by the provided record flag.
  ! -------------------------------------------------------------------------- !
  subroutine initChosenWriter(recordFlag, saveFilename, newWriterUnit)
    character,        intent(in)    :: recordFlag
      !! Record flag. Values can be found in `WriterOptions`.
    character(len=*), intent(in)    :: saveFilename
      !! Directory of the file to be written on.
    integer,          intent(in)    :: newWriterUnit
      !! Fortran ID for the file to be written on.

    character(len=15), allocatable :: headerArr(:)
    type(Writer), pointer :: chosenWriter
    integer :: i, startingAge

    chosenWriter => getWriterPtr(recordFlag)

    if (.not.associated(chosenWriter)) return
    if (initWriterArr( scan(REC_FLAG_ORDER, recordFlag) )) then
      call raiseError(                                                &
          "Record flag '"// recordFlag // "' occured more than once." &
        )
    end if

    ! Initialize the chosen data writer.
    chosenWriter = Writer(saveFilename, newWriterUnit, delim)
    ! Open file for writing.
    call chosenWriter%openFile()

    ! Append the header to the file to be written on.
    select case (recordFlag)
      case (REC_POP)
        ! Data description.
        call chosenWriter%write("DATA: Population size per time step")

        ! Header of the list.
        if (divider == DIVIDER_READABLE) & 
            call chosenWriter%write([divider])
        call chosenWriter%write(["Population size"])
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([divider])
      

      case(REC_GENE_DSTRB, REC_AGE_DSTRB)
        ! Data description.
        if (recordFlag == REC_AGE_DSTRB) then
          call chosenWriter%write("DATA: Age distribution per time step")
          startingAge = 0  ! Get the starting age of the distribution as well.
        else
          call chosenWriter%write("DATA: Bad gene distribution per time step")
          startingAge = 1  ! Get the starting age of the distribution as well.
        end if
        
        ! NOTE: For some reason, implicit do loop truncate numbers.
        allocate(headerArr(startingAge:MODEL_L))
        do i = startingAge, MODEL_L
          headerArr(i) = "AGE " // trim(castIntToChar(i))
        end do

        ! Header of the table.
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([(divider, i = startingAge, MODEL_L)])
        call chosenWriter%write(headerArr)
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([(divider, i = startingAge, MODEL_L)])


      case (REC_DEATH)
        ! Data description.
        call chosenWriter%write("DATA: Number of deaths " // &
            "(due to old age, mutation, Verhulst factor) per time step")
        
        ! Header of the table.
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([(divider, i = 1, 3)])
        call chosenWriter%write( &
            ["Old age        ", &
             "Mutation       ", &
             "Verhulst factor"])
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([(divider, i = 1, 3)])
        

      case (REC_DIV_IDX)
        ! Data description. 
        call chosenWriter%write("DATA: Genetic diversity index per time step.")

        ! Header of the list.
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([divider])
        call chosenWriter%write(["Diversity idx"])
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([divider])
      

      case (REC_TIME)
        call chosenWriter%write("DATA: Timing statistics")
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([(divider, i = 1, 5)])
        call chosenWriter%write( &
          ["Max Time Step ", &
           "Init pop size ", &
           "Sample size   ", &
           "Avg. time (ms)", &
           "Std. dev. (ms)"])
        if (divider == DIVIDER_READABLE) &
           call chosenWriter%write([(divider, i = 1, 5)])


      case (REC_GNM_COUNT)
        call chosenWriter%write("DATA: Number of unique genome per time step.")
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([divider])
        call chosenWriter%write("Unique genome count")
        if (divider == DIVIDER_READABLE) &
            call chosenWriter%write([divider])
      

      case (REC_NULL)
        ! Do nothing 
        return


      case default
        call raiseError("'" // recordFlag //"' is an invalid record flag")
    end select

    if (allocated(headerArr)) deallocate(headerArr)
    ! Mark the specified writer as initialized.
    initWriterArr( scan(REC_FLAG_ORDER, recordFlag) ) = .true.
  end subroutine initChosenWriter


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: appendToFilename
  !>  Append a string to the body of file name, i.e. not to the file extension.
  ! -------------------------------------------------------------------------- !
  function appendToFilename(filename, appended) result(appendedFilename)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: appended

    character(len=:), allocatable :: fileExt
    character(len=:), allocatable :: appendedFilename
    character :: currChar
    integer :: i
    integer :: filenameLen
    logical :: copyingFileExt

    allocate(character(len=0) :: fileExt, appendedFilename)

    filenameLen = len(filename)
    copyingFileExt = .true.
    do i = filenameLen, 1, -1
      currChar = filename(i: i)
      
      if (copyingFileExt) then
        fileExt = currChar // fileExt
      else
        appendedFilename = currChar // appendedFilename
      end if

      if (currChar == ".") copyingFileExt = .false.
    end do

    appendedFilename = appendedFilename // appended // fileExt
  end function appendToFilename


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isWriterInitialized
  !>  Check whether at least one of the specified writer is initialized.
  ! -------------------------------------------------------------------------- !
  pure logical function isWriterInitialized(recordFlags)
    character(len=*), intent(in) :: recordFlags
    integer :: writerIdx
    integer :: i

    isWriterInitialized = .false.
    do i = 1, len(recordFlags)
      writerIdx = scan(REC_FLAG_ORDER, recordFlags(i:i))
  
      if (writerIdx > 0) then
        isWriterInitialized = initWriterArr(writerIdx)
      end if

      if (isWriterInitialized) exit
    end do
  end function isWriterInitialized


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setDividerDelimChar
  !>  Set the divider and delim strings based on the chosen format.
  ! -------------------------------------------------------------------------- !
  subroutine setDividerDelimChar(inCSVFormat)
    logical, intent(in) :: inCSVFormat
    
    if (inCSVFormat) then
      delim = DELIM_CSV
      allocate(character(len=0) :: divider)
    else
      delim = DELIM_READABLE
      divider = DIVIDER_READABLE
    end if

    dividerDelimSet = .true.
  end subroutine setDividerDelimChar


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: flagMatchesDataWriterType
  !>  Return TRUE if the input record Flag matches the input data writer type.
  ! -------------------------------------------------------------------------- !
  logical function flagMatchesDataWriterType(recordFlag, dataWriterType)
    character, intent(in) :: recordFlag
    integer,   intent(in) :: dataWriterType
  
    flagMatchesDataWriterType = .false.
    select case (dataWriterType)
    case (DATA_WRITER_PENNA)
      flagMatchesDataWriterType = (scan(REC_FLAG_PENNA, recordFlag) > 0)
    case (DATA_WRITER_PROG)
      flagMatchesDataWriterType = (scan(REC_FLAG_PROG, recordFlag) > 0)
    case default
      call raiseError("Internal error. Unknown Data writer type.")
    end select
  end function flagMatchesDataWriterType

  
  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initDataWriter
  !>  Initialize data writers of the specified type.
  ! -------------------------------------------------------------------------- !
  subroutine initDataWriter(                                   &
        recordFlags, saveFilename, inCSVFormat, dataWriterType &
      )
    character(len=*), intent(in) :: recordFlags
    character(len=*), intent(in) :: saveFilename
    logical,          intent(in) :: inCSVFormat
    integer,          intent(in) :: dataWriterType

    character(len=:), allocatable :: newSaveFilename
    character :: currFlag
    integer   :: recordFlagLen
    integer   :: i
    
    if (.not. dividerDelimSet) call setDividerDelimChar(inCSVFormat)

    recordFlagLen = len(recordFlags)
    if (recordFlagLen == 0) then
      call raiseError("Internal error. Empty record flag")
    end  if

    do i = 1, recordFlagLen
      currFlag = recordFlags(i: i)

      ! Skip flags that are not of the specified type.
      if (.not.flagMatchesDataWriterType(currFlag, dataWriterType)) then
        cycle
      end if

      ! Append a unique indentifier to file names when saving multiple data sets
      if (recordFlagLen > 1) then
        if (allocated(newSaveFilename)) deallocate(newSaveFilename)
        newSaveFilename = appendToFilename(saveFilename, &
                                           "_(flag=" // currFlag // ")")
      else
        newSaveFilename = saveFilename
      end if

      call initChosenWriter(currFlag, newSaveFilename, BASE_UNIT + i)
    end do
  end subroutine initDataWriter


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: closeDataWriter
  !>  Close the data writers of the specified type.
  ! -------------------------------------------------------------------------- !
  subroutine closeDataWriter(dataWriterType)
    integer, intent(in) :: dataWriterType

    type(Writer), pointer :: currWriterPtr
    character :: currFlag
    integer   :: i

    do i = 1, len(REC_FLAG_ORDER)
      currFlag = REC_FLAG_ORDER(i:i)

      if (.not. flagMatchesDataWriterType(currFlag, dataWriterType)) then
        cycle
      end if

      currWriterPtr => getWriterPtr(currFlag)
      if (currWriterPtr%isFileOpen()) then
         call currWriterPtr%closeFile()
      end if

      initWriterArr( scan(REC_FLAG_ORDER, currFlag) ) = .false.
    end do
  end subroutine closeDataWriter
end module DataWriter
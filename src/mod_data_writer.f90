module DataWriter
  ! -------------------------------------------------------------------------- !
  ! MODULE:  DataWriter
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for writing data onto files.
  ! -------------------------------------------------------------------------- !
  use Parameters, only:  &
    MODEL_L,             &
    MODEL_ENTROPY_ORDER, &
    REC_NULL,            &
    REC_POP,             &
    REC_AGE_DSTRB,       &
    REC_DEATH,           &
    REC_DIV_IDX,         &
    REC_GENE_DSTRB,      &
    REC_GNM_COUNT,       &
    REC_TIME,            &
    REC_FLAG_PENNA,      &
    REC_FLAG_PROG,       &
    REC_FLAG_ORDER
  use ErrorMSG, only: raiseError
  use WriterType, only: Writer_t, init_Writer, writeIK, writeRK
  use CastProcs, only: castCharToInt, castIntToChar, castRealToChar, isFinite
  implicit none
  private

  ! Divider string
  character(len=*), parameter   :: DIVIDER = "---------------"
  character(len=:), allocatable :: dividerArr(:)
    !! Array of dividers for certain multi-dimensional data 
  
  ! Data file delimiters.
  character, parameter :: DELIM_READABLE = "|"
  character, parameter :: DELIM_CSV = ","
  character :: delim = achar(0)

  ! File name formatter
  character(len=*), parameter :: FILE_NAME_FMT_DELIM       = "%"
  character(len=*), parameter :: FILE_NAME_FLAG_FMT        = "f"
  character(len=*), parameter :: FILE_NAME_DATASET_NUM_FMT = "n"
  character(len=*), parameter :: FILE_NAME_FLAG_PAD        = "_"
  character(len=*), parameter :: FILE_NAME_DATASET_NUM_PAD = "0"

  ! File delimiter and divider string status.
  logical :: isInCSVFormat = .false.
  logical :: dividerDelimSet = .false.
  
  ! Data writer types.
  integer, parameter :: DATA_WRITER_PENNA = 0
  integer, parameter :: DATA_WRITER_PROG  = 1

  ! All writer objects.
  type(Writer_t), target  :: writerArr(len(REC_FLAG_ORDER))
  logical :: initWriterArr(len(REC_FLAG_ORDER)) = .false.

  public :: initDataWriter
  public :: getWriterPtr
  public :: isWriterInitialized
  public :: closeDataWriter
  
  public :: DATA_WRITER_PENNA
  public :: DATA_WRITER_PROG

  public :: Writer_t
  public :: writeIK
  public :: writeRK
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: isNumber
  !>  Returns TRUE if the input character is a number, otherwise FALSE.
  ! -------------------------------------------------------------------------- !
  pure logical function isNumber(c)
    character, intent(in) :: c
    integer :: cint

    cint = iachar(c)
    isNumber = (48 <= cint .and. cint <= 57)
  end function isNumber


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: evalOutFileNameFmt
  !>  Parse the output file name and substitute in the specified data.
  ! -------------------------------------------------------------------------- !
  subroutine evalOutFileNameFmt(srcStr, outStr, recFlag, dataSetNum)
    character(len=*),              intent(in)    :: srcStr
    character(len=:), allocatable, intent(inout) :: outStr
    character,                     intent(in)    :: recFlag
    integer,                       intent(in)    :: dataSetNum
    integer   :: i, fmtPos, parsedInt
    character :: c

    character(len=:), allocatable :: dataSetNumStr
    integer :: dataSetNumStrLen

    if (allocated(outStr)) deallocate(outStr)
    allocate(character(len=0) :: outStr)

    dataSetNumStr = castIntToChar(dataSetNum)
    dataSetNumStrLen = len(dataSetNumStr)

    parsedInt = 0
    i = 0
    do while (i < len(srcStr))
      i = i + 1
      c = srcStr(i:i)

      if (c /= FILE_NAME_FMT_DELIM) then
        outStr = outStr // c
        cycle
      end if

      i = i + 1
      fmtPos = i
      c = srcStr(i:i)

      if (isNumber(c)) then
        do while( isNumber(srcStr(i:i)) )
          i = i + 1
        end do
        parsedInt = castCharToInt( srcStr(fmtPos:i-1) )
        c = srcStr(i:i)
      end if

      select case(c)
      case (FILE_NAME_FLAG_FMT)
        if (dataSetNumStrLen < parsedInt) then
          outStr = outStr // &
              repeat(FILE_NAME_FLAG_PAD, parsedInt - dataSetNumStrLen)
        end if
        outStr = outStr // recFlag

      case (FILE_NAME_DATASET_NUM_FMT)
        if (dataSetNumStrLen < parsedInt) then
          outStr = outStr // &
              repeat(FILE_NAME_DATASET_NUM_PAD, parsedInt - dataSetNumStrLen)
        end if
        outStr = outStr // dataSetNumStr

      case default
        outStr = outStr // FILE_NAME_FMT_DELIM
        i = fmtPos - 1
        cycle
      end select

      parsedInt = 0
    end do

    deallocate(dataSetNumStr)
  end subroutine evalOutFileNameFmt


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getWriterPtr
  !>  Get a pointer to the writer object specified with a record flag.
  ! -------------------------------------------------------------------------- !
  function getWriterPtr(recordFlag) result(writerPtr)
    character, intent(in) :: recordFlag
    type(Writer_t), pointer :: writerPtr
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
  subroutine initChosenWriter(recordFlag, saveFilename)
    character,        intent(in)    :: recordFlag
      !! Record flag. Values can be found in `WriterOptions`.
    character(len=*), intent(in)    :: saveFilename
      !! Directory of the file to be written on.

    type(Writer_t), pointer :: chosenWriter
    integer :: i, startingAge

    chosenWriter => getWriterPtr(recordFlag)

    if (.not.associated(chosenWriter)) return
    if (initWriterArr( scan(REC_FLAG_ORDER, recordFlag) )) then
      call raiseError(                                                &
          "Record flag '"// recordFlag // "' occured more than once." &
        )
    end if

    ! Initialize the chosen data writer.
    call init_Writer(chosenWriter, saveFilename, delim)
    ! Open file for writing.
    call chosenWriter%openFile()

    ! Append the header to the file to be written on.
    select case (recordFlag)
      case (REC_POP)
        ! Data description.
        call chosenWriter%write("DATA: Population size per time step")

        ! Header of the list.
        if (.not.isInCSVFormat) & 
            call chosenWriter%write(DIVIDER)
        call chosenWriter%write("Population size")
        if (.not.isInCSVFormat) &
            call chosenWriter%write(DIVIDER)
      

      case(REC_GENE_DSTRB, REC_AGE_DSTRB)
      recDstrb: block
        character(len=15), allocatable :: headerArr(:)

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
        if (.not.isInCSVFormat) &
            call chosenWriter%write(dividerArr(startingAge:MODEL_L))
        call chosenWriter%write(headerArr)
        if (.not.isInCSVFormat) &
            call chosenWriter%write(dividerArr(startingAge:MODEL_L))
          
        deallocate(headerArr)
      end block recDstrb


      case (REC_DEATH)
        ! Data description.
        call chosenWriter%write("DATA: Number of deaths " // &
            "(due to old age, mutation, Verhulst factor) per time step")
        
        ! Header of the table.
        if (.not.isInCSVFormat) &
            call chosenWriter%write(dividerArr(1:3))
        call chosenWriter%write( &
            ["Old age        ", &
             "Mutation       ", &
             "Verhulst factor"])
        if (.not.isInCSVFormat) &
            call chosenWriter%write(dividerArr(1:3))
        

      case (REC_DIV_IDX)
      divIdx: block
        character(len=:), allocatable :: entOrderStr

        if (isFinite(MODEL_ENTROPY_ORDER)) then
          entOrderStr = castRealToChar(MODEL_ENTROPY_ORDER)
        else
          entOrderStr = "1.0 (Normalized)"
        end if

        ! Data description. 
        call chosenWriter%write(                            &
          "DATA: Renyi Entropy of order " // entOrderStr // &
          " as genetic diversity per time step."            &
        )

        ! Header of the list.
        if (.not.isInCSVFormat) &
            call chosenWriter%write(DIVIDER)
        call chosenWriter%write("Genetic diversity index")
        if (.not.isInCSVFormat) &
            call chosenWriter%write(DIVIDER)

        deallocate(entOrderStr)
      end block divIdx
      

      case (REC_TIME)
        call chosenWriter%write("DATA: Timing statistics")
        if (.not.isInCSVFormat) &
            call chosenWriter%write(dividerArr(1:5))
        call chosenWriter%write( &
          ["Max Time Step ", &
           "Init pop size ", &
           "Sample size   ", &
           "Avg. time (ms)", &
           "Std. dev. (ms)"])
        if (.not.isInCSVFormat) &
           call chosenWriter%write(dividerArr(1:5))


      case (REC_GNM_COUNT)
        call chosenWriter%write("DATA: Number of unique genome per time step.")
        if (.not.isInCSVFormat) &
            call chosenWriter%write(DIVIDER)
        call chosenWriter%write("Unique genome count")
        if (.not.isInCSVFormat) &
            call chosenWriter%write(DIVIDER)
      

      case (REC_NULL)
        ! Do nothing 
        return


      case default
        call raiseError("'" // recordFlag //"' is an invalid record flag")
    end select

    ! Mark the specified writer as initialized.
    initWriterArr( scan(REC_FLAG_ORDER, recordFlag) ) = .true.
  end subroutine initChosenWriter


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
    else
      delim = DELIM_READABLE
      allocate(   &
          character(len=len(DIVIDER)) :: dividerArr(0:MODEL_L)  &
        )
      dividerArr(:) = DIVIDER
    end if

    dividerDelimSet = .true.
    isInCSVFormat = inCSVFormat
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
  subroutine initDataWriter(                                               &
        recordFlags, saveFilename, inCSVFormat, dataWriterType, dataSetNum &
      )
    character(len=*), intent(in) :: recordFlags
    character(len=*), intent(in) :: saveFilename
    logical,          intent(in) :: inCSVFormat
    integer,          intent(in) :: dataWriterType
    integer,          intent(in) :: dataSetNum

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

      ! Substitute the specified formatter in the provided save file name.
      call evalOutFileNameFmt( &
          trim(saveFileName), newSaveFileName, currFlag, dataSetNum &
        )

      call initChosenWriter(currFlag, newSaveFilename)

      deallocate(newSaveFilename)
    end do
  end subroutine initDataWriter


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: closeDataWriter
  !>  Close the data writers of the specified type.
  ! -------------------------------------------------------------------------- !
  subroutine closeDataWriter(dataWriterType)
    integer, intent(in) :: dataWriterType

    type(Writer_t), pointer :: currWriterPtr
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
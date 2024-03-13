module DataWriter
  ! -------------------------------------------------------------------------- !
  ! MODULE:  DataWriter
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing procedures for writing data onto files.
  ! -------------------------------------------------------------------------- !
  use Parameters, only: MODEL_L
  use CastProcs, only: castIntToChar
  use ErrorMSG, only: raiseError
  use WriterType, only: Writer, writeIK, writeRK
  implicit none
  private

  character(len=*), parameter   :: DIVIDER_READABLE = "---------------"
  character(len=:), allocatable :: divider

  ! Data file delimiters
  character, parameter :: DELIM_READABLE = "|"
  character, parameter :: DELIM_CSV = ","
  character :: delim = char(0)

  ! Record flags.
  character, parameter, public :: REC_NULL = "x"
    !! Nothing (do not record).
  character, parameter, public :: REC_POP = "p"
    !! Population size per time step.
  character, parameter, public :: REC_AGE_DSTRB = "a"
    !! Age distribution in the last 300 time steps
  character, parameter, public :: REC_DEATH = "d"
    !! Death counts (death by age, by mutation, by Verhulst factor) 
    !! per time step.
  character, parameter, public :: REC_DIV_IDX = "s"
    !! Genetic diversity index per time step (Normalized Shannon index)
  character, parameter, public :: REC_GENE_DSTRB = "b"
    !! Bad gene distribution per time step.
  character, parameter, public :: REC_TIME = "t"
    !! (Average) elapsed time and standard deviation if applicable.
  character, parameter, public :: REC_GNM_COUNT = "c"
    !! Number of unique genomes per time step.
  character(len=*), parameter  :: REC_FLAG_ORDER = "padsbtc"

  ! All writer objects.
  type(Writer), target  :: writerArr(len(REC_FLAG_ORDER))
  logical :: initWriterArr(len(REC_FLAG_ORDER)) = .false.

  public :: initDataWriter
  public :: getWriterPtr
  public :: isWriterInitialized
  public :: Writer
  public :: writeIK
  public :: writeRK
contains


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getWriterIdx
  !>  Get the index that corresponds with the specified record flag.
  ! -------------------------------------------------------------------------- !
  pure function getWriterIdx(recordFlag) result(writerIdx)
    character, intent(in) :: recordFlag
    integer   :: writerIdx, i
    character :: currFlag

    writerIdx = -1

    do i = 1, len(REC_FLAG_ORDER)
      currFlag = REC_FLAG_ORDER(i: i)

      if (currFlag == recordFlag) then
        writerIdx = i
        exit
      end if
    end do
  end function getWriterIdx


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getWriterPtr
  !>  Get a pointer to the writer object specified with a record flag.
  ! -------------------------------------------------------------------------- !
  function getWriterPtr(recordFlag) result(writerPtr)
    character, intent(in) :: recordFlag
    type(Writer), pointer :: writerPtr
    integer :: writerIdx

    writerIdx = getWriterIdx(recordFlag)
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
    if (initWriterArr(getWriterIdx(recordFlag))) then
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
    initWriterArr(getWriterIdx(recordFlag)) = .true.
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
      writerIdx = getWriterIdx(recordFlags(i: i))
  
      if (writerIdx > 0) then
        isWriterInitialized = initWriterArr(writerIdx)
      end if

      if (isWriterInitialized) exit
    end do
  end function isWriterInitialized

  
  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initDataWriter
  !>  Initialize all the data/output writers.
  ! -------------------------------------------------------------------------- !
  subroutine initDataWriter(recordFlags, saveFilename, inCSVFormat)
    character(len=*), intent(in) :: recordFlags
    character(len=*), intent(in) :: saveFilename
    logical,          intent(in) :: inCSVFormat

    integer, parameter :: baseUnit = 99
    character :: currFlag
    integer :: recordFlagLen
    integer :: i
    
    character(len=:), allocatable :: newSaveFilename

    if (inCSVFormat) then
      delim = DELIM_CSV
      allocate(character(len=0) :: divider)
    else
      delim = DELIM_READABLE
      divider = DIVIDER_READABLE
    end if

    recordFlagLen = len(recordFlags)
    if (recordFlagLen == 0) then
      call raiseError("Internal error. Empty record flag")
    end  if

    do i = 1, recordFlagLen
      currFlag = recordFlags(i: i)

      ! Append a unique indentifier to file names when saving multiple data sets
      if (recordFlagLen > 1) then
        if (allocated(newSaveFilename)) deallocate(newSaveFilename)
        newSaveFilename = appendToFilename(saveFilename, &
                                           "_(flag=" // currFlag // ")")
      else
        newSaveFilename = saveFilename
      end if

      call initChosenWriter(currFlag, newSaveFilename, baseUnit + i)
    end do
  end subroutine initDataWriter
end module DataWriter
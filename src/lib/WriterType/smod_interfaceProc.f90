submodule (WriterType) interfaceProcedures
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: interfaceProcedures
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `WriterType` containing public procedures with their
  !!  private "helper" procedures for interfacing with other modules, program,
  !!  or other procedures.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: declareAvailableFiles
  !>  Declare the available data and the corresponding output files to save 
  !!  into.
  ! -------------------------------------------------------------------------- !
  subroutine declareAvailableFiles(files)
    type(OutputFile), intent(in) :: files(:)
      !! Available output files.
  
    if (allocated(outputFiles)) deallocate(outputFiles)

    allocate(outputFiles(size(files)))
    outputFiles(:) = files(:)
  end subroutine declareAvailableFiles  


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: constructWriter_array
  !>  Create and initialize, if `initialize` is true, a new `Writer` object
  !!  with list of file information as `OutputFile` objects. 
  ! -------------------------------------------------------------------------- !
  subroutine constructWriter_array(new, files, initialize)
    type(Writer),      intent(inout) :: new
      !! Newly initialized `Writer` object. 
    type(OutputFile),  intent(in)    :: files(:)
      !! Files available to be written on.
    logical, optional, intent(in)    :: initialize
      !! Initialize `new` with all its available output files set to active.

    integer :: i

    ! Allocate allocatable attributes.
    if (allocated(new % availableFiles)) deallocate(new % availableFiles)
    if (allocated(new % activeFiles)) deallocate(new % activeFiles)

    ! Assign new flags 
    do i = 1, size(files)
      call constructWriter_scalar(new, files(i))
    end do

    ! Activate all available flags. False by default.
    if (present(initialize)) then
      if (initialize) then
        call new % initialize
      end if
    end if
  end subroutine constructWriter_array


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: constructWriter_scalar
  !>  Create and initialize, if `initialize` is true, a new `Writer` object
  !!  with file information as an `OutputFile` object. 
  ! -------------------------------------------------------------------------- !
  subroutine constructWriter_scalar(new, file, initialize)
    type(Writer),      intent(inout) :: new
      !! Newly initialized `Writer` object.
    type(OutputFile),  intent(in)    :: file
      !! Files available to be written on.
    logical, optional, intent(in)    :: initialize
      !! Initialize `new` with all its available output files set to active.

    if (allocated(outputFiles)) then
      ! Check for redundant flags first.
      if (allocated(new % availableFiles)) then
        if (any(new % availableFiles % flag == file % flag)) then
          call raiseError("Initializing a 'Writer' object with " // &
              "redundant file flags: '" //  file % flag // "'")
        end if
      end if

      ! Add the new file to the new `Writer` object.
      call appendOutputFile(new % availableFiles, file)

      ! Activate all available flags. False by default.
      if (present(initialize)) then
        if (initialize) call new % initialize()
      end if
    else
      call raiseError("Available output files are not yet declared.")
    end if
  end subroutine constructWriter_scalar


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: removeFilebyFlag
  !>  Remove an `OutputFile` object from an allocatable array of the same type.
  ! -------------------------------------------------------------------------- !
  subroutine removeFilebyFlag(array, flag)
    type(OutputFile), allocatable, intent(inout) :: array(:)
      !! Array of `OutputfFiles` to be modified.
    character,                     intent(in)    :: flag
      !! Flags the corresponding flag of which is to be removed.
  
    type(OutputFile), allocatable :: tempArray(:)
    integer :: oldSize
    integer :: i
    logical :: fileRemoved

    if (allocated(array)) then
      ! Search the array one by one for the file we hope to remove.
      fileRemoved = .false.
      do i = 1, size(array)

        ! Compare flags since flags are unique.
        if (array(i) % flag == flag) then
          ! Save the size of the old file.
          oldSize = size(array)

          ! Reallocate array with smaller size.
          call move_alloc(array, tempArray)
          allocate(array(oldSize - 1))

          ! Copy all elements in `tempArray` except the file to be removed.
          array(:(i - 1)) = tempArray(:(i - 1))
          array((i + 1):) = tempArray((i + 1):)

          deallocate(tempArray)
          fileRemoved = .true.
          exit
        end if
      end do

      ! Error handling.
      if (.not. fileRemoved) then
        call raiseError("'OutputFile' object to remove as " // &
            "specified by the flag '" //  flag // "' is not found.")
      end if
    end if
  end subroutine removeFilebyFlag


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: removeFilebyFlag
  !>  Remove an `OutputFile` object from an allocatable array of the same type.
  ! -------------------------------------------------------------------------- !
  subroutine appendOutputFile(array, file)
    type(OutputFile), allocatable, intent(inout) :: array(:)
      !! Array of `Outputfile` to be modified.
    type(OutputFile),              intent(in)    :: file
      !! `OutputFile` object to be appended to `array`.
  
    type(OutputFile), allocatable :: tempArray(:)
    integer :: oldSize

    if (allocated(array)) then
      ! Save the old array size.
      oldSize = size(array)

      ! Allocate new space for the `file` to append.
      call move_alloc(array, tempArray)
      allocate(array(oldSize + 1))

      ! Append `file` at the end.
      array(1:oldSize) = tempArray(:)
      array(oldSize + 1) = file

      deallocate(tempArray)
    else
      allocate(array(1))
      array(1) = file
    end if
  end subroutine appendOutputFile


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: findFileByFlag
  !>  Return an `OutputFile` object from an array of the same type that matches
  !!  the integer `flag`.
  ! -------------------------------------------------------------------------- !
  subroutine findFileByFlag(array, flag, foundFile)
    type(OutputFile), allocatable, intent(in)    :: array(:)
      !! `Writer` object whose files are to be searched.
    type(OutputFile), allocatable, intent(inout) :: foundFile
      !! The sought file. Unallocaeted if no file with the given `flag` is
      !! found. 
    character, intent(in) :: flag
      !! Flag of the corresponding sought file.

    integer :: i

    if (allocated(array)) then
      if (allocated(foundFile)) deallocate(foundFile)

      do i = 1, size(array)
        if (array(i) % flag == flag) then
          foundFile = array(i) ! NOTE: Automatic allocation.
          exit
        end if
      end do
    else
      call raiseError("The provided array to find flags with " // &
          "is not yet allocated.")
    end if
  end subroutine findFileByFlag


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeWriterModAlloctbls
  !>  Deallocate any module allocatable variables.
  ! -------------------------------------------------------------------------- !
  subroutine freeWriterModAlloctbls()
    if (allocated(outputFiles)) deallocate(outputFiles)
  end subroutine freeWriterModAlloctbls


  ! -------------------------------------------------------------------------- !
  ! BOUND SUBROUTINE: [Writer%]destroy
  !>  Deallocate the allocatable attributes `enabledFlags`
  !!  and `liveFlags`.
  ! -------------------------------------------------------------------------- !
  subroutine destroy(self)
    type(Writer), intent(inout) :: self
      !! `Writer` object to be destroyed.

    if (allocated(self % availableFiles)) deallocate(self % availableFiles)
    if (allocated(self % activeFiles)) deallocate(self % activeFiles)
  end subroutine destroy
end submodule
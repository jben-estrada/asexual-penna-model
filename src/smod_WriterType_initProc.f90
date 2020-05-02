
submodule (WriterType) WriterTypeInitProc
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: WriterTypeInitProc
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `WriterType` containing the specific procedures for the
  !!  generic type-bound procedure `[Writer] % initialize`.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  subroutine initializeFile(file)
    type(OutputFile) :: file
      !! Output file to initialize.

    logical :: exists

    inquire(file=file % filename, exist=exists)

    if (exists) then
      open(file % unit, file=file % filename, status="old", &
        position=file % position)
    else
      open(file % unit, file=file % filename, status="new")
    end if
  end subroutine initializeFile


  subroutine writer_initializeAll(self)
    class(Writer), intent(inout) :: self
      !! `Writer` type.
    integer :: i

    if (allocated(self % availableFiles)) then
      if (size(self % availableFiles) == 0) return
    else
      call raiseError("'availableFiles' attribute is not yet allocated.")
    end if
    
    ! Initialize all available files in `self`.
    do i = 1, size(self % availableFiles)
      call initializeFile(self % availableFiles(i))
    end do

    ! Put all enabled flags into live flags
    if(allocated(self % activeFiles)) deallocate(self % activeFiles)
    allocate(self % activeFiles(size(self % availableFiles)))

    self % activeFiles(:) = self % availableFiles(:)
  end subroutine writer_initializeAll


  subroutine writer_initialize(self, flag)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    character, intent(in)        :: flag
      !! Flag corresponding to the output file to be written.

    type(OutputFile), allocatable :: foundFile

    if (allocated(self % availableFiles)) then
      ! Search for the file corresponding to the given flag.
      call findFileByFlag(self % availableFiles, flag, foundFile)
    else
      call raiseError("'availableFiles' attribute is not yet allocated.")
    end if

    if (allocated(foundFile)) then
      call initializeFile(foundFile)

      ! Set the found file as active for writing.
      if (allocated(self % activeFiles)) deallocate(self % activeFiles)
      allocate(self % activeFiles(1))
      self % activeFiles = [foundFile]

      deallocate(foundFile)
    else
      call raiseError("The flag '" //  flag // &
          "' does not correspond to any recordable data set.")
    end if
  end subroutine writer_initialize


  subroutine writer_listInitialize(self, flags)
    class(Writer), intent(inout) :: self
      !! `Writer` object to be modified.
    character,     intent(in)    :: flags(:)
      !! Array of flags corresponding to the output files to be written.

    type(OutputFile), allocatable :: foundFile
    integer :: i

    if (allocated(self % availableFiles)) then
      if(allocated(self % activeFiles)) deallocate(self % activeFiles)
      allocate(self % activeFiles(0))
  
      ! Check the flags if its corresponding file is available.
      do i = 1, size(flags)
        call findFileByFlag(self % availableFiles, flags(i), foundFile)
  
        if (allocated(foundFile)) then
          call initializeFile(foundFile)
          
          !! Insert active file.
          call appendOutputFile(self % activeFiles, foundFile)
        end if
      end do
    end if
  end subroutine writer_listInitialize
end submodule WriterTypeInitProc

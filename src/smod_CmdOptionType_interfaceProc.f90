submodule (CmdOptionType) interfaceProcedures
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: interfaceProcedures
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `CmdOptionType` containing public procedures with their
  !!  private "helper" procedures for interfacing with other modules, program,
  !!  or other procedures.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeCmdOption
  !>  Initialize a command-line option.
  ! -------------------------------------------------------------------------- !
  subroutine initializeCmdOption(cmdOption, command, altCommand)
    class(BaseCmdOption),       intent(out) :: cmdOption
      !! Command-line option to initialize.
    character(len=*),           intent(in)  :: command
      !! Command character to assign to `command` attribute of `cmdOption`.
    character(len=*), optional, intent(in)  :: altCommand
    !! Alternat command to assign to `altCommand` attribute of `cmdOption`.

    cmdOption % command = command
    if (present(altCommand)) then
      cmdOption % altCommand = altCommand
    else
      cmdOption % altCommand = NULL_CHAR  
    end if
  end subroutine initializeCmdOption


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parsePassedCmdArgs
  !>  Parse command-line arguments with the provided valid command-line options.
  ! -------------------------------------------------------------------------- !
  subroutine parsePassedCmdArgs(cmdFlags, cmdKeyVal, cmdPosArgs, &
      readFlag, readKeyVal, readPosArg)
    class(FlagCmdOption),       intent(inout) :: cmdFlags(:)
      !! Command-line flags to be modified by passed cmd arguments.
    class(KeyValCmdOption),     intent(inout) :: cmdKeyVal(:)
      !! Command-line key-value options to be modified by passed cmd arguments.
    class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
      !! Positional command-line options to be modified by passed cmd arguments.
    logical,                    intent(in)    :: readFlag
      !! Read and assign passed flags.
    logical,                    intent(in)    :: readKeyVal
      !! Read and assign passed key-value options.
    logical,                    intent(in)    :: readPosArg
      !! Read and assign passed positional arguments.

    integer :: argCount
    integer :: status
    character(len=MAX_LEN) :: cmdArg

    do argCount = 1, command_argument_count()
      call get_command_argument(argCount, cmdArg, status=status)

      if (cmdArg == NULL_CHAR) cycle

      call toggleFlagOptions(cmdFlags, cmdArg, status, readFlag)
      if (status == 0) cycle

      call assignKeyValOption(cmdKeyVal, cmdArg, status, readKeyVal)
      if (status == 0) cycle

      call assignPositionalArg(cmdPosArgs, cmdArg, status, readPosArg)
      if (status /= 0) then
        print "(3a)", "***ERROR. '", trim(cmdArg), "' is not a valid option."
        stop
      end if
    end do

    ! Check for missing values.
    if (readKeyVal) call checkUninitializedValues(cmdKeyVal)
    if (readPosArg) call checkUninitializedValues(cmdPosArgs)
  end subroutine parsePassedCmdArgs


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: compareCommand
  !>  Compare command and alternative command with the passed command-line
  !!  argument.
  ! -------------------------------------------------------------------------- !
  logical pure function compareCommand(cmdOption, cmdArg)
    class(BaseCmdOption), intent(in) :: cmdOption
      !! Command-line option whose `command` attribute is to compared with.
    character(len=*),     intent(in) :: cmdArg
      !! Command character to be compared with `cmdOption`.

    compareCommand = cmdOption % command == cmdArg &
        .or. cmdOption % altCommand == cmdArg
  end function compareCommand


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: toggleFlagOptions
  !>  Toggle the flag option matching with the passed command-line argument.
  ! -------------------------------------------------------------------------- !
  subroutine toggleFlagOptions(cmdFlags, cmdArg, status, toRead)
    class(FlagCmdOption), intent(inout) :: cmdFlags(:)
      !! Command-line flags to be modified.
    character(len=*),     intent(in)    :: cmdArg
      !! Passed command-line argument.
    integer,              intent(out)   :: status
      !! Status of this routine. Return non-zero value to signify failure.
      !! Return `0` if reading and assigning succeeds.
    logical,              intent(in)    :: toRead
      !! Check validity of passed argument but do not read and assign.

    integer :: i

    ! NOTE: Non-zero values for 'status' mean the routine failed.
    status = 1

    do i = 1, size(cmdFlags)
      if (compareCommand(cmdFlags(i), cmdArg)) then
        ! Mark the routine to be successful in finding a syntactically-valid
        ! value.
        status = 0

        ! Check if the current flag option has already been toggled.
        if (toRead .and. .not. cmdFlags(i) % isToggled) then
          cmdFlags(i) % value = invertFlagChar(cmdFlags(i) % value)
          cmdFlags(i) % hasValue = .true.
          cmdFlags(i) % isToggled = .true.
        end if
        exit
      end if
    end do
  end subroutine toggleFlagOptions


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: incertFlagChar
  !>  Invert the given `flagChar` character. More precisely "T" -> "F" and
  !!  "F" -> "T".
  ! -------------------------------------------------------------------------- !
  character pure function invertFlagChar(flagChar)
    character(len=*), intent(in) :: flagChar
      !! Flag character. It must be either "T" or "F".

    if (flagChar == TRUE_FLAG) then
      invertFlagChar = FALSE_FLAG
    else
      invertFlagChar = TRUE_FLAG
    end if
  end function invertFlagChar


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignKeyValOption
  !>  Assign the value for the matching key-value option.
  ! -------------------------------------------------------------------------- !
  subroutine assignKeyValOption(cmdKeyVal, cmdArg, status, toRead)
    class(KeyValCmdOption), intent(inout) :: cmdKeyVal(:)
      !! Command-line key-value options to be modified.
    character(len=*),       intent(in)    :: cmdArg
      !! Passed command-line argument.
    integer,                intent(out)   :: status
      !! Status of this routine. Return non-zero value to signify failure.
      !! Return `0` if reading and assigning succeeds.
    logical,                intent(in)    :: toRead
      !! Check validity of passed argument but do not read and assign.

    character(len=MAX_LEN) :: key
    character(len=MAX_LEN) :: value
    integer :: i

    ! Get key and value from 'cmdArg' char.
    call getKeyVal(cmdArg, key, value, status)
    if (status /= 0) return
    
    status = 1
    do i = 1, size(cmdKeyVal)
      if (compareCommand(cmdKeyVal(i), key)) then
        ! Mark the routine to be successful in finding a syntactically-valid
        ! value.
        status = 0

        if (toRead) then
          cmdKeyVal(i) % value = value
          cmdKeyVal(i) % hasValue = .true.
        end if
        exit
      end if
    end do
  end subroutine assignKeyValOption


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignPositionalArg
  !>  Assign positional arguments.
  ! -------------------------------------------------------------------------- !
  subroutine assignPositionalArg(cmdPosArgs, cmdArg, status, toRead)
    class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
      !! Positional command-line option.
    character(len=*),           intent(in)    :: cmdArg
      !! Passed command-line argument.
    integer,                    intent(out)   :: status
      !! Status of this routine. Return non-zero value to signify failure.
      !! Return `0` if reading and assigning succeeds.
    logical,                    intent(in)    :: toRead
      !! Check validity of passed argument but do not read and assign.

    integer, save :: posCount = 1
    integer :: i

    status = 1
    do i = 1, size(cmdPosArgs)
      if (cmdPosArgs(i) % position == posCount) then
        ! Mark the routine to be successful in finding a syntactically-valid
        ! value.
        status = 0

        if (toRead) then
          cmdPosArgs(i) % value = cmdArg
          cmdPosArgs(i) % hasValue = .true.
          posCount = posCount + 1
        end if

        exit
      end if
    end do
  end subroutine assignPositionalArg


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getKeyVal
  !>  Get the key and value characters from the provided cmd argument.
  ! -------------------------------------------------------------------------- !
  subroutine getKeyVal(cmdArg, key, value, status)
    character(len=*),       intent(in)  :: cmdArg
      !! Raw command-line argument.
    character(len=MAX_LEN), intent(out) :: key
      !! Key character obtained from the given command-line argument. 
    character(len=MAX_LEN), intent(out) :: value
      !! Value character obtained from the given command-line argument.
    integer,                intent(out) :: status
      !! Status of this routine. Return non-zero value to signify failure.
      !! Return `0` if reading and assigning succeeds.

    integer   :: i
    character :: currChar
    logical   :: isReadingKey
    character(len=:), allocatable :: tempStr

    ! Initialize outputs.
    ! NOTE: Non-zero values for status mean this routine fails.
    status = 0
    key = NULL_CHAR
    value = NULL_CHAR

    ! Initialize local variables.
    isReadingKey = .true.
    allocate(character(len=0) :: tempStr)

    do i = 1, len(cmdArg)
      currChar = cmdArg(i:i)

      if (currChar == KEY_VAL_SEP) then
        isReadingKey = .false.
        key = tempStr
        tempStr = NULL_CHAR
      else
        tempStr = tempStr // currChar
      end if
    end do

    if (isReadingKey) then
      ! Mark the routine as failed since no "=" is detected.
      status = 1
    else
      ! Get the value string.
      value = tempStr
    end if
    if (allocated(tempStr)) deallocate(tempStr)
  end subroutine getKeyVal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkUninitializedValues
  !>  Check for command-line options with missing values.
  ! -------------------------------------------------------------------------- !
  subroutine checkUninitializedValues(cmdOptions)
    class(BaseCmdOption), intent(in) :: cmdOptions(:)
      !! Command-line options to be checked for uniinitialized values.

    integer :: i

    do i =  1, size(cmdOptions)
      if (.not. cmdOptions(i) % hasValue) then
        print "(3a)", "***ERROR. The command '", trim(cmdOptions(i) % command),&
            "' requires a value." 
        stop
      end if
    end do
  end subroutine checkUninitializedValues


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: showHelpMsg
  !>  Show the help messages with the provided command-line options.
  ! -------------------------------------------------------------------------- !
  subroutine showHelpMsg(cmdFlags, cmdKeyVal, cmdPosArgs)
    class(FlagCmdOption),       intent(in) :: cmdFlags(:)
      !! Command-line flags.
    class(KeyValCmdOption),     intent(in) :: cmdKeyVal(:)
      !! Command-line key-value options.
    class(PositionalCmdOption), intent(in) :: cmdPosArgs(:)
      !! Positional command-line options.

    character(len=:), allocatable :: tempChar
    integer :: tempCharLen
    integer :: i

    ! Print the header.
    write(*, "(a)", advance="no") "usage: penna.out [options]"
    do i = 1, size(cmdPosArgs)
      if (cmdPosArgs(i) % isOptional) write(*, "(a)", advance="no") " ["
      write(*, "(a)", advance="no") trim(cmdPosArgs(i) % command)
      if (cmdPosArgs(i) % isOptional) write(*, "(a)", advance="no") "]"
    end do

    print "(//a)", "options:"
    ! Print the usage message for flags.
    do i = 1, size(cmdFlags)
      tempChar = trim(cmdFlags(i) % command) // " " // &
          trim(cmdFlags(i) % altCommand)

      tempCharLen = 25*(1 + len(tempChar)/25)
      print "(4(' '), 2a)", [character(len=tempCharLen) :: tempChar], &
          trim(cmdFlags(i) % usageMsg)
    end do

    ! Print the usage message for key-value options.
    do i = 1, size(cmdKeyVal)
      tempChar = trim(cmdKeyVal(i) % command) // &
          "=" // trim(cmdKeyVal(i) % valueMsg)
        
      if (cmdKeyVal(i) % altCommand /= NULL_CHAR) then
        tempChar = tempChar // " " // trim(cmdKeyVal(i) % altCommand) // &
            "=" // trim(cmdKeyVal(i) % valueMsg)
      end if

      tempCharLen = 25*(1 + len(tempChar)/25)
      write(*, "(4(' '), 2a)", advance="no") &
          [character(len=tempCharLen) :: tempChar], &
          trim(cmdKeyVal(i) % usageMsg)
      
      if (cmdKeyVal(i) % isOptional) then
        print "(*(a))", " [", trim(adjustl(cmdKeyVal(i) % value)), "]"
      else
        print *, ""
      end if
    end do

    ! Print the positional arguments.
    do i = 1, size(cmdPosArgs)
      tempChar = trim(cmdPosArgs(i) % command) // " " // &
          trim(cmdPosArgs(i) % altCommand)

      tempCharLen = 25*(1 + len(tempChar)/25)
      write(*, "(4(' '), 2a)", advance="no") &
          [character(len=tempCharLen) :: tempChar], &
          trim(cmdPosArgs(i) % usageMsg)
      
      if (cmdPosArgs(i) % isOptional) then
        print "(3a)", " [", trim(cmdPosArgs(i) % value), "]"
      else
        print *, ""
      end if
    end do

    deallocate(tempChar)
  end subroutine showHelpMsg
end submodule

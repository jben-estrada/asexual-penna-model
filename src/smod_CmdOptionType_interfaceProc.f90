submodule (CmdOptionType) interfaceProcedures
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: interfaceProcedures
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `CmdOptionType` containing public procedures with their
  !!  private "helper" procedures for interfacing with other modules, program,
  !!  or other procedures.
  ! -------------------------------------------------------------------------- !
  use ErrorMSG, only: raiseError
  implicit none

  character(len=*), parameter :: shortCommDelim = "-"
  character(len=*), parameter :: longCommDelim  = "--"
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeCmdOption
  !>  Initialize a command-line option.
  ! -------------------------------------------------------------------------- !
  subroutine initializeCmdOption(cmdOption, command, shortCommand)
    class(BaseCmdOption),       intent(out) :: cmdOption
      !! Command-line option to initialize.
    character(len=*),           intent(in)  :: command
      !! Command character to assign to `command` attribute of `cmdOption`.
    character(len=*), optional, intent(in)  :: shortCommand
      !! Alternate command to assign to `shortCommand` attribute of `cmdOption`.

    cmdOption % command = command
    if (present(shortCommand)) then
      cmdOption % shortCommand = shortCommand
    else
      cmdOption % shortCommand = NULL_CHAR  
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

    character(len=MAX_LEN) :: cmdArg
    character(len=MAX_LEN) :: remainingArg

    integer :: argCount     ! Integer for indexing command-line arguments.
    integer :: posCount     ! Integer for indexing positional arguments.
    integer :: maxArgCount  ! Total command-line argument count.
    integer :: status       ! Read and assign status.

    ! Initialize local variables.
    argCount = 0  ! NOTE: Initialized with 0 since it is used in 'do while'
    posCount = 1
    maxArgCount = command_argument_count()
    if (maxArgCount == 0) return

    do
      ! Exit condition.
      if (argCount == maxArgCount) then
        exit
      else
        argCount = argCount + 1
      end if
      
      call get_command_argument(argCount, cmdArg, status=status)
      
      ! Filter out empty characters.
      if (cmdArg == NULL_CHAR) cycle

      status = 1
      ! Parse full command, i.e. commands starting with '--'.
      if (len(trim(cmdArg)) > len(longCommDelim)) then
        if (cmdArg(1:len(longCommDelim)) == longCommDelim) then
          ! Check long flag options.
          call parseLongCommand(cmdFlags, cmdArg(len(longCommDelim) + 1:), &
              readFlag, status)
          ! Go to the next argument if parsing succeeds, else evaluate
          ! argument as a key-value command and not a flag.
          if (status == 0) cycle

          ! Check full key-value command.
          call parseLongCommand(cmdKeyVal, cmdArg(len(longCommDelim) + 1:), &
              readKeyVal, status)

          ! Go to the next argument if parsing succeeds.
          if (status == 0) then
            cycle

          ! Raise error if no match was found.
          else
            call raiseError("'" // trim(cmdArg) // "' is an invalid command.")
          end if
        end if
      end if

      ! Parse short command, i.e. commands starting with a single '-' char.
      if (len(trim(cmdArg)) > len(shortCommDelim)) then
        if (cmdArg(1:len(shortCommDelim)) == shortCommDelim) then
          ! Check short flag options.
          call parseShortCommand(cmdFlags, cmdArg(len(shortCommDelim) + 1:), &
              remainingArg, argCount, readFlag)

          ! Check short key-value command-line options.
          ! NOTE: We use `cmdArg` here to hold the remaining unremoved char.
          !       Declaring another char variable would just be a waste of time
          !       and space.
          call parseShortCommand(cmdKeyVal, remainingArg, cmdArg, argCount, &
              readKeyVal)

          ! Go to the next argument if parsing succeeds.
          ! NOTE: `cmdArg` holds the last remaining unmatched char.
          if (len(trim(cmdArg)) == 0) then
            cycle
          else
            ! Raise an error if there is at least one non-matching character in 
            ! `cmdArg`.
            call raiseError("'" // shortCommDelim // remainingArg(1:1) // &
                "' is an invalid command.")
          end if
        end if
      end if

      ! Parse argument as a positional argument.
      call assignPositionalArg(cmdPosArgs, cmdArg, status, posCount, readPosArg)

      ! Raise an error if the passed argument failed to match any defined
      ! command-line options.
      if (status /= 0) &
        call raiseError("'" // trim(cmdArg) // "' is an invalid command.")
    end do

    ! Check for missing values. Raise an error if at least one has no value.
    ! NOTE: Optional commands already have default values.
    if (readKeyVal) call checkUninitializedValues(cmdKeyVal)
    if (readPosArg) call checkUninitializedValues(cmdPosArgs)
  end subroutine parsePassedCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseShortCommand
  !>  Parse `cmdArg` as either a short flag command or a key-value command.
  ! -------------------------------------------------------------------------- !
  subroutine parseShortCommand(cmdOptions, cmdArg, remainingArg, argCount, &
        toRead)
    class(BaseCmdOption),   intent(inout) :: cmdOptions(:)
      !! Command-line options to be modified.
    character(len=*),       intent(in)    :: cmdArg
      !! Passed command-line argument.
    character(len=MAX_LEN), intent(out)   :: remainingArg
      !! Characters in `cmdArg` with matching substrings removed.
    integer,                intent(inout) :: argCount
      !! Current count of command-line arguments that have been parsed.
      !! This would index if a short key-value argument is passed.
    logical,                intent(in)    :: toRead  
      !! Evaluate and assign command-line flags. If false, evaluate only but
      !! do not assign values.

    character(len=:), allocatable :: mangledArg
    character(len=:), allocatable :: currChar

    integer, parameter :: FLAG_TYPE = 0
    integer, parameter :: KV_TYPE = 1

    integer :: cmdOptionType
    integer :: cmdArgLen
    integer :: commCount
    integer :: charCount
    integer :: idxOffset
    logical :: matchFound

    ! Get the class of `cmdOptions`.
    select type(cmdOptions)
      class is (FlagCmdOption)
        cmdOptionType = FLAG_TYPE

      class is (KeyValCmdOption)
        cmdOptionType = KV_TYPE

      class default
        call raiseError("Invalid 'BaseCmdOption' type extension." // &
            " It must be 'KeyValCmdOption'.")
    end select
    
    ! Initialize local variables.
    allocate(character(len=0) :: currChar)
    allocate(character(len=0) :: mangledArg)
    cmdArgLen = len(trim(cmdArg))
    charCount = 1
    idxOffset = 0

    ! Find matches in command-line options.
    do
      if (charCount > cmdArgLen) exit

      matchFound = .false.
      ! Loop through command-line options to look for a match.
      do commCount = 1, size(cmdOptions)
        ! Get substring the same length as with the current flag.
        idxOffset = len(trim(cmdOptions(commCount) % shortCommand)) - 1

        ! Check length before checking the current command for a match.
        ! It's a shame Fortran 2008 does not have short-circuit evaluation.
        if (charCount + idxOffset <= cmdArgLen) then
          ! Get substring for convenience.
          currChar = cmdArg(charCount: charCount + idxOffset)

          ! Assign value to matching command if `toRead` is true.
          if (cmdOptions(commCount) % shortCommand == currChar) then
            matchFound = .true.

            ! Run type-specific procedures in assigning values to matching
            ! commands. This is ignored if explicitly specified not to read
            ! assign values.
            select case (cmdOptionType)
              case (FLAG_TYPE)
                call toggleFlagOption(cmdOptions(commCount), toRead)

              case (KV_TYPE)
                call assignShortKVOption(cmdOptions(commCount), argCount, &
                    toRead)

              case default
                call raiseError("Unknown 'BaseCmdOption' type extension.")
            end select

            ! Exit out of the inner loop to ignore potentially redundant
            ! commands.
            exit
          end if
        else
          cycle
        end if

      end do
      ! Go to the next substring.
      if (matchFound) then
        charCount = charCount + idxOffset + 1

      ! If no match was found for the current substring, append it to
      ! `remainingArg`.
      else
        mangledArg = mangledArg // cmdArg(charCount: charCount)
        charCount = charCount + 1
      end if
    end do

    remainingArg = adjustl(mangledArg)

    ! Deallocate allocatables just to be sure.
    if (allocated(mangledArg)) deallocate(mangledArg)
    if (allocated(currChar)) deallocate(currChar)
  end subroutine parseShortCommand


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: toggleFlagOption
  !>  Toggle the provided flag command, i.e. invert the default boolean value.
  ! -------------------------------------------------------------------------- !
  subroutine toggleFlagOption(cmdOption, toRead)
    class(BaseCmdOption), intent(inout) :: cmdOption
      !! Flag option to toggle.
    logical,              intent(in)    :: toRead
      !! Assign the obtained command-line argument.

    ! Filter out `cmdOption` of unwanted type.
    select type(cmdOption)
      class is (FlagCmdOption)
        ! Prevent multiple toggling.
        if (.not. cmdOption % isToggled .and. toRead) then
          cmdOption % value = invertFlagChar(cmdOption % value)
          cmdOption % hasValue = .true.
          cmdOption % isToggled = .true.
        end if

      class default
        call raiseError("Only 'FlagCmdOption' objects can be toggled.")
    end select
  end subroutine toggleFlagOption


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignShortKVOption
  !>  Parse and assign value of the matching command obtained from the passed
  !!  argument. This assumes that the matching command is the short command.
  ! -------------------------------------------------------------------------- !
  subroutine assignShortKVOption(cmdOption, valIdx, toRead)
    class(BaseCmdOption), intent(inout) :: cmdOption
      !! Key-value option whose value is to be modified.
    integer,              intent(inout) :: valIdx
      !! Index of command-line argument for the value.
    logical,              intent(in)    :: toRead
      !! Assign the obtained command-line argument.

    character(len=MAX_LEN) :: value
    integer :: status
  
    ! Filter out `cmdOption` of unwanted type.
    select type(cmdOption)
      class is (KeyValCmdOption)
        ! Go to the next command-line argument.
        valIdx = valIdx + 1

        ! Get the value for `cmdOption`.
        call get_command_argument(valIdx, value, status=status)

        ! Accept value if getting succeeds.
        if (status == 0) then
          ! Assign value.
          if (toRead) then
            cmdOption % value = adjustl(value)
            cmdOption % hasValue = .true.
          end if
        else
          call raiseError("Value for the command '" // shortCommDelim // &
              trim(cmdOption % shortCommand) // "' cannot be obtained.")
        end if

      class default
        call raiseError("Invalid 'BaseCmdOption' type extension." // &
            " It must be 'KeyValCmdOption'.")
    end select
  end subroutine assignShortKVOption


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignLongKVOption
  !>  Parse and assign value of the matching command obtained from the passed
  !!  argument. This assumes that the matching command is the full command.
  ! -------------------------------------------------------------------------- !
  subroutine assignLongKVOption(cmdOption, cmdArg, toRead, status)
    class(BaseCmdOption), intent(inout) :: cmdOption
      !! Key-value option whose value is to be modified.
    character(len=*),     intent(in)    :: cmdArg
    logical,              intent(in)    :: toRead
    integer,              intent(out)   :: status
      !! Status of this routine.

    character(len=MAX_LEN) :: key
    character(len=MAX_LEN) :: value

    ! Filter out `cmdOption` of unwanted type.
    select type(cmdOption)
      class is (KeyValCmdOption)
      ! Separate the key part and value part of the command-line argument.
      call getKeyVal(cmdArg, key, value, status)

      ! Check if parsing succeeds.
      if (status == 0) then
        ! Check if the successfully parsed argument does match `cmdOption`.
        if (cmdOption % command == key) then
          if (toRead) then
            cmdOption % value = value
            cmdOption % hasValue = .true.
          end if
        else
          status = 1
        end if
      end if

      class default
        call raiseError("Invalid 'BaseCmdOption' type extension." // &
            " It must be 'KeyValCmdOption'.")
    end select
  end subroutine assignLongKVOption


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseLongCommand
  !>  Parse `cmdArg` as either a full flag command or a key-value command.
  ! -------------------------------------------------------------------------- !
  subroutine parseLongCommand(cmdOptions, cmdArg, toRead, status)
    class(BaseCmdOption),   intent(inout) :: cmdOptions(:)
      !! Command-line options to be modified.
    character(len=*),       intent(in)    :: cmdArg
      !! Command-line argument
    logical,                intent(in)    :: toRead
      !! Evaluate and assign command-line flags. If false, evaluate only but
      !! do not assign values.
    integer,                intent(out)   :: status
      !! Status of this routine. A value of 0 signifies that this routine has
      !! found a matching command in `cmdOptions`. Other values mean it has
      !! failed.

    integer, parameter :: FLAG_TYPE = 0
    integer, parameter :: KV_TYPE = 1

    integer :: cmdOptionType
    integer :: commCount
  
    ! Get the class of `cmdOptions`.
    select type(cmdOptions)
      class is (FlagCmdOption)
        cmdOptionType = FLAG_TYPE

      class is (KeyValCmdOption)
        cmdOptionType = KV_TYPE

      class default
        call raiseError("Invalid 'BaseCmdOption' class. It must be " // &
            "either 'FlagCmdOption' or 'KeyValCmdOption'.")
    end select

    ! Pessimistically initialize `status` to 1.
    ! NOTE: Non-zero value for `status` means the routine failed to find a match
    status = 1
    do commCount = 1, size(cmdOptions)
      ! Select the procedure for the approprirate type.
      select case (cmdOptionType)
        case (FLAG_TYPE)
          if (cmdOptions(commCount) % command == cmdArg) then
            status = 0
            call toggleFlagOption(cmdOptions(commCount), toRead)
            exit
          end if

        case (KV_TYPE)
          call assignLongKVOption(cmdOptions(commCount), cmdArg, toRead, status)
          ! Value for `status` is determined in `assignLongKVOption`.
          if (status == 0) exit

        case default
          call raiseError("Unknown 'BaseCmdOption' type extension.")
      end select
    end do
  end subroutine parseLongCommand


  ! -------------------------------------------------------------------------- !
  ! FUNCTION: invertFlagChar
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
  ! SUBROUTINE: assignPositionalArg
  !>  Assign positional arguments.
  ! -------------------------------------------------------------------------- !
  subroutine assignPositionalArg(cmdPosArgs, cmdArg, status, posCount, toRead)
    class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
      !! Positional command-line option.
    character(len=*),           intent(in)    :: cmdArg
      !! Passed command-line argument.
    integer,                    intent(inout) :: status
      !! Status of this routine. Return non-zero value to signify failure.
      !! Return `0` if reading and assigning succeeds.
    integer,                    intent(inout) :: posCount
      !! Valid position argument count.
    logical,                    intent(in)    :: toRead
      !! Check validity of passed argument but do not read and assign.

    integer :: i

    ! Pessimistically set `status` as failed.
    do i = 1, size(cmdPosArgs)
      if (cmdPosArgs(i) % position == posCount) then
        ! Mark the routine to be successful in finding a syntactically-valid
        ! value.
        status = 0

        if (toRead) then
          cmdPosArgs(i) % value = cmdArg
          cmdPosArgs(i) % hasValue = .true.
        end if

        posCount = posCount + 1
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

      ! Switch from LHS to RHS once "=" is encountered.
      if (currChar == KEY_VAL_SEP) then
        isReadingKey = .false.
        key = tempStr

        ! Reallocate `tempStr` with empty character.
        deallocate(tempStr)
        allocate(character(len=0) :: tempStr)
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
      if (.not. cmdOptions(i) % hasValue) &
        call raiseError("The command '" // trim(cmdOptions(i) % command) // &
        "' requires a value.")
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
    integer :: i

    ! Print the header.
    write(*, "(a)", advance="no") "usage: penna.out [options...]"
    do i = 1, size(cmdPosArgs)
      if (cmdPosArgs(i) % isOptional) write(*, "(a)", advance="no") " ["
      write(*, "(a)", advance="no") trim(cmdPosArgs(i) % command)
      if (cmdPosArgs(i) % isOptional) write(*, "(a)", advance="no") "]"
    end do

    print "(//a)", "options:"
    ! Print the usage message for flags.
    do i = 1, size(cmdFlags)
      print "(4(' '), a10, a20, a)", shortCommDelim // &
          cmdFlags(i) % shortCommand, longCommDelim // cmdFlags(i) % command, &
          trim(cmdFlags(i) % usageMsg)
    end do

    ! Print the usage message for key-value options.
    do i = 1, size(cmdKeyVal)
      ! Print the short command.
      if (cmdKeyVal(i) % shortCommand == NULL_CHAR) then
        tempChar = ""
      else
        tempChar = shortCommDelim // trim(cmdKeyVal(i) % shortCommand) // &
            " " // trim(cmdKeyVal(i) % valueMsg)
      end if
      write(*, "(4(' '), a10)", advance="no") [character(len=10) :: tempChar]
      
      ! Print the full command plus the usage text.
      tempChar = longCommDelim // trim(cmdKeyVal(i) % command) // "=" // &
          trim(cmdKeyVal(i) % valueMsg)
      write (*, "(a20, a)", advance="no") [character(len=20) :: tempChar], &
          trim(cmdKeyVal(i) % usageMsg)
      
      ! Print the default value if the command is optional.
      if (cmdKeyVal(i) % isOptional) then
        print "(' [default: ', a, ']')", trim(adjustl(cmdKeyVal(i) % value))
      else
        print *, ""
      end if
    end do

    ! Print the positional arguments.
    do i = 1, size(cmdPosArgs)
      write(*, "(4(' '), a30, a)", advance="no") cmdPosArgs(i) % command, &
          trim(cmdPosArgs(i) % usageMsg)

      if (cmdPosArgs(i) % isOptional) then
        print "(' [default: ', a, ']')", trim(adjustl(cmdPosArgs(i) % value))
      else
        print *, ""
      end if
    end do

    deallocate(tempChar)
  end subroutine showHelpMsg
end submodule

submodule (CmdOptionType) InterfaceProcedures
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeCmdOption
  !>  Initialize a command-line option.
  ! -------------------------------------------------------------------------- !
  subroutine initializeCmdOption(cmdOption, command, altCommand)
    implicit none
    
    class(BaseCmdOption),       intent(out) :: cmdOption
    character(len=*),           intent(in)  :: command
    character(len=*), optional, intent(in)  :: altCommand

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
    implicit none

    class(FlagCmdOption),       intent(inout) :: cmdFlags(:)
    class(KeyValCmdOption),     intent(inout) :: cmdKeyVal(:)
    class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
    logical,                    intent(in)    :: readFlag
    logical,                    intent(in)    :: readKeyVal
    logical,                    intent(in)    :: readPosArg

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
  logical function compareCommand(cmdOption, cmdArg)
    implicit none

    class(BaseCmdOption), intent(in) :: cmdOption
    character(len=*),     intent(in) :: cmdArg

    compareCommand = cmdOption % command == cmdArg &
        .or. cmdOption % altCommand == cmdArg
  end function compareCommand


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: toggleFlagOptions
  !>  Toggle the flag option matching with the passed command-line argument.
  ! -------------------------------------------------------------------------- !
  subroutine toggleFlagOptions(cmdFlags, cmdArg, status, toRead)
    implicit none

    class(FlagCmdOption), intent(inout) :: cmdFlags(:)
    character(len=*),     intent(in)    :: cmdArg
    integer,              intent(out)   :: status
    logical,              intent(in)    :: toRead

    integer :: i

    ! NOTE: Non-zero values for 'status' mean the routine failed.
    status = 1

    do i = 1, size(cmdFlags)
      if (compareCommand(cmdFlags(i), cmdArg)) then
        status = 0

        if (.not. toRead) exit

        ! Check if the current flag option has already been toggled.
        if (.not. cmdFlags(i) % hasValue) then
          cmdFlags(i) % state = .not. cmdFlags(i) % state
          cmdFlags(i) % hasValue = .true.
          exit
        end if
      end if
    end do
  end subroutine toggleFlagOptions


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignKeyValOption
  !>  Assign the value for the matching key-value option.
  ! -------------------------------------------------------------------------- !
  subroutine assignKeyValOption(cmdKeyVal, cmdArg, status, toRead)
    implicit none
  
    class(KeyValCmdOption), intent(inout) :: cmdKeyVal(:)
    character(len=*),       intent(in)    :: cmdArg
    integer,                intent(out)   :: status
    logical,                intent(in)    :: toRead

    character(len=MAX_LEN) :: key
    character(len=MAX_LEN) :: valueChar
    integer :: i

    ! Get key and value from 'cmdArg' char.
    call getKeyVal(cmdArg, key, valueChar, status)
    if (status /= 0) return
    
    status = 1
    do i = 1, size(cmdKeyVal)
      if (compareCommand(cmdKeyVal(i), key)) then
        call assignValueTo(cmdKeyVal(i), valueChar, toRead)
        status = 0
        exit
      end if
    end do
  end subroutine assignKeyValOption


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignValueTo
  !>  Assign value to the specified key-value option.
  ! -------------------------------------------------------------------------- !
  subroutine assignValueTo(cmdOption, valueChar, toRead)
    implicit none
    
    class(KeyValCmdOption), intent(inout) :: cmdOption
    character(len=*),       intent(in)    :: valueChar
    logical,                intent(in)    :: toRead

    integer :: valueInt
    integer :: status

    read(valueChar, *, iostat=status) valueInt

    if (status == 0) then

      if (.not. toRead) return

      cmdOption % value = valueInt
      cmdOption % hasValue = .true.
    else
      print "(3a)", "***ERROR. '", trim(cmdOption % command), &
          "' only accepts integers."
      stop
    end if
  end subroutine assignValueTo


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignPositionalArg
  !>  Assign positional arguments.
  ! -------------------------------------------------------------------------- !
  subroutine assignPositionalArg(cmdPosArgs, cmdArg, status, toRead)
    implicit none

    class(PositionalCmdOption), intent(inout) :: cmdPosArgs(:)
    character(len=*),           intent(in)    :: cmdArg
    integer,                    intent(out)   :: status
    logical,                    intent(in)    :: toRead

    integer, save :: posCount = 1
    integer :: i

    status = 1
    do i = 1, size(cmdPosArgs)
      if (cmdPosArgs(i) % position == posCount) then
        status = 0
        if (.not. toRead) exit

        cmdPosArgs(i) % value = cmdArg
        cmdPosArgs(i) % hasValue = .true.
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
    implicit none

    character(len=*),       intent(in)  :: cmdArg
    character(len=MAX_LEN), intent(out) :: key
    character(len=MAX_LEN), intent(out) :: value
    integer,                intent(out) :: status

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
      ! Mark the routine "failed" since no "=" is detected.
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
    implicit none
    class(BaseCmdOption), intent(in) :: cmdOptions(:)

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
    implicit none

    class(FlagCmdOption),       intent(in) :: cmdFlags(:)
    class(KeyValCmdOption),     intent(in) :: cmdKeyVal(:)
    class(PositionalCmdOption), intent(in) :: cmdPosArgs(:)

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
        print "(a, i0, a)", " [", cmdKeyVal(i) % value, "]"
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
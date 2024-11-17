submodule (Parameters) CmdArgAssignProcs
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: CmdArgAssignProcs
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `Parameters` containing public procedures with their
  !!  private "helper" procedures for interfacing with other modules, program,
  !!  or other procedures.
  ! -------------------------------------------------------------------------- !
  implicit none


  type :: CmdArgRecord
    !! A handy derived type for collecting command option attributes.
    character(len=:), allocatable :: cmdName
      !! Name of the command.
    character(len=:), allocatable :: cmdAlias
      !! Alias for `cmdName`.
    character :: cmdType
      !! Command type of `cmdName`.
    character :: cmdAliasType
      !! Command type of `cmdAlias`.

    character(len=:), allocatable :: usageTxt
      !! Usage text to be displayed in the help message.

    character(len=MAX_LEN), pointer :: charValue_ptr => null()
      !! Pointer to the character parameter `cmdName` modifies.
    integer,                pointer :: intValue_ptr => null()
      !! Pointer to the integer parameter `cmdName` modifies.
    real,                   pointer :: realValue_ptr => null()
      !! Pointer to the real parameter `cmdName` modifies
  end type

  ! Command line arguments
  ! -------------------------------------------------------------------------- !
  ! Array of all commands
  type(CmdArgRecord) :: cmdArgArr(21)

  ! Model parameters.
  integer, parameter :: IDX_MTTN_THRESHOLD_KV  = 1
  integer, parameter :: IDX_BIRTH_RATE_KV      = 2
  integer, parameter :: IDX_MTTN_RATE_KV       = 3
  integer, parameter :: IDX_R_AGE_MIN_KV       = 4
  integer, parameter :: IDX_R_AGE_MAX_KV       = 5
  integer, parameter :: IDX_MAX_POP_SIZE_KV    = 6
  integer, parameter :: IDX_INIT_MTTN_COUNT_KV = 7   ! special values: -1, 0
  integer, parameter :: IDX_INIT_POP_SIZE_KV   = 8
  integer, parameter :: IDX_MAX_TIME_STEP_KV   = 9
  integer, parameter :: IDX_ENTROPY_ORDER_KV   = 10  ! can be any floats
  integer, parameter :: IDX_AGE_DSTRB_TIME_KV  = 11
  
  ! Program parameters.
  integer, parameter :: IDX_SAMPLE_SIZE_KV     = 12
  integer, parameter :: IDX_RECORD_DATA_KV     = 13  ! String-valued command
  integer, parameter :: IDX_RNG_CHOICE_KV      = 14  ! Special constraint value
  integer, parameter :: IDX_RNG_SEED_KV        = 15  ! Any integer
  integer, parameter :: IDX_PARAM_FILE_PATH_KV = 16  ! String-valued command
  integer, parameter :: IDX_OUT_FILE_PATH_KV   = 17  ! String-valued command
  integer, parameter :: IDX_SHOW_PARAM_F       = 18
  integer, parameter :: IDX_NO_PARAM_F         = 19
  integer, parameter :: IDX_SHOW_VERSION_F     = 20
  integer, parameter :: IDX_CSV_FORMAT_F       = 21

  ! Commands whose value must be positive integers.
  integer, parameter :: CMD_POSITIVE_INT(*) = [ &
    IDX_MTTN_THRESHOLD_KV, &
    IDX_BIRTH_RATE_KV,     &
    IDX_MTTN_RATE_KV,      &
    IDX_R_AGE_MIN_KV,      &
    IDX_R_AGE_MAX_KV,      &
    IDX_MAX_POP_SIZE_KV,   &
    IDX_INIT_POP_SIZE_KV,  &
    IDX_MAX_TIME_STEP_KV,  &
    IDX_AGE_DSTRB_TIME_KV, &
    IDX_SAMPLE_SIZE_KV     &
  ]
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initCmdArgRecs
  !>  Initialize `CmdArgRecord` objects in the `cmdArgArr` array.
  ! -------------------------------------------------------------------------- !
  subroutine initCmdArgRecs(pennaCmdArgs)
    type(CmdArgParser), target, intent(inout) :: pennaCmdArgs
      !! Command argument parser for this program.
    integer :: i

    ! Model parameters.
    cmdArgArr(IDX_MTTN_THRESHOLD_KV) = CmdArgRecord(      &
        "T", "mttn-lim", KV_S, KV_L, "Mutation threshold" &
      )
    cmdArgArr(IDX_BIRTH_RATE_KV) = CmdArgRecord(     &
        "B", "birth-rate", KV_S, KV_L, "Birth rate"  &
      )
    cmdArgArr(IDX_MTTN_RATE_KV) = CmdArgRecord(        &
        "M", "mttn-rate", KV_S, KV_L, "Mutation rate"  &
      )
    cmdArgArr(IDX_R_AGE_MIN_KV) = CmdArgRecord(                  &
        "r", "r-age-min", KV_S, KV_L, "Minimum reproduction age" &
      )
    cmdArgArr(IDX_R_AGE_MAX_KV) = CmdArgRecord(                   &
        "R", "r-age-max", KV_S, KV_L, "Maximum reproduction age"  &
      )
    cmdArgArr(IDX_MAX_POP_SIZE_KV) = CmdArgRecord(       &
        "K", "pop-cap", KV_S, KV_L, "Carrying capacity"  &
      )
    cmdArgArr(IDX_INIT_MTTN_COUNT_KV) = CmdArgRecord(                          &
        "m", "init-mttn", KV_S, KV_L, "Initial mutation count per individual"  &
      )
    cmdArgArr(IDX_INIT_POP_SIZE_KV) = CmdArgRecord(             &
        "p", "pop-size", KV_S, KV_L, "Initial population size"  &
      )
    cmdArgArr(IDX_MAX_TIME_STEP_KV) = CmdArgRecord(        &
        "t", "time-step", KV_S, KV_L, "Maximum time step"  &
      )
    cmdArgArr(IDX_ENTROPY_ORDER_KV) = CmdArgRecord(          &
        "O", "ent-order", KV_S, KV_L, "Renyi entropy order"  &
      )
    cmdArgArr(IDX_AGE_DSTRB_TIME_KV) = CmdArgRecord(          &
        "a", "age-dstrb-time", KV_S, KV_L,                    &
        "Age distribution time step till the final time step" &
      )

    ! Program parameters.
    cmdArgArr(IDX_SAMPLE_SIZE_KV) = CmdArgRecord(      &
        "s", "sample-size", KV_S, KV_L, "Sample size"  &
      )
    cmdArgArr(IDX_RECORD_DATA_KV) = CmdArgRecord(                 &
        "d", "record-data", KV_S, KV_L,                           &
        "Record data. See 'notes' below for recordable data set"  &
      )
    cmdArgArr(IDX_RNG_CHOICE_KV) = CmdArgRecord(                      &
        "g", "rng-choice", KV_S, KV_L,                                &
        "Choice of pseduo-RNG. See 'notes' below for available RNGs"  &
      )
    cmdArgArr(IDX_RNG_SEED_KV) = CmdArgRecord(                    &
        "S", "rng-seed", KV_S, KV_L, "RNG seed. Must be integer"  &
      )
    cmdArgArr(IDX_PARAM_FILE_PATH_KV) = CmdArgRecord(                &
        "f", "param-file", KV_S, KV_L,                               &
        "Path to the file containing the default parameter listing"  &
      )
    cmdArgArr(IDX_OUT_FILE_PATH_KV) = CmdArgRecord(                      &
        "o", "out", KV_S, KV_L, "Path to file data is to be written in"  &
      )
    cmdArgArr(IDX_SHOW_PARAM_F) = CmdArgRecord(              &
      "v", "verbose", FLAG_S, FLAG_L, "Show all parameters"  &
    )
    cmdArgArr(IDX_NO_PARAM_F) = CmdArgRecord(      &
        "q", "quiet", FLAG_S, FLAG_L,              &
        "Print nothing when running this program"  &
      )
    cmdArgArr(IDX_SHOW_VERSION_F) = CmdArgRecord(                           &
        "V", "version", FLAG_S, FLAG_L, "Show the version of this program"  &
      )
    cmdArgArr(IDX_CSV_FORMAT_F) = CmdArgRecord(     &
        "c", "csv-format", FLAG_S, FLAG_L,          &
        "Set the output files to be in CSV format"  &
      )

    ! Assign pointers.
    cmdArgArr(IDX_MTTN_THRESHOLD_KV)  % intValue_ptr  => MODEL_T
    cmdArgArr(IDX_BIRTH_RATE_KV)      % intValue_ptr  => MODEL_B
    cmdArgArr(IDX_MTTN_RATE_KV)       % intValue_ptr  => MODEL_M
    cmdArgArr(IDX_R_AGE_MIN_KV)       % intValue_ptr  => MODEL_R
    cmdArgArr(IDX_R_AGE_MAX_KV)       % intValue_ptr  => MODEL_R_MAX
    cmdArgArr(IDX_MAX_POP_SIZE_KV)    % intValue_ptr  => MODEL_K
    cmdArgArr(IDX_INIT_MTTN_COUNT_KV) % intValue_ptr  => MODEL_MTTN_COUNT
    cmdArgArr(IDX_INIT_POP_SIZE_KV)   % intValue_ptr  => MODEL_START_POP_SIZE
    cmdArgArr(IDX_MAX_TIME_STEP_KV)   % intValue_ptr  => MODEL_TIME_STEPS
    cmdArgArr(IDX_ENTROPY_ORDER_KV)   % realValue_ptr => MODEL_ENTROPY_ORDER
    cmdArgArr(IDX_AGE_DSTRB_TIME_KV)  % intValue_ptr  &
      => MODEL_AGE_DSTRB_INIT_TIMESTEP

    cmdArgArr(IDX_SAMPLE_SIZE_KV) % intValue_ptr => PROG_SAMPLE_SIZE
    cmdArgArr(IDX_RNG_CHOICE_KV)  % intValue_ptr => PROG_RNG
    cmdArgArr(IDX_RNG_SEED_KV)    % intValue_ptr => PROG_RNG_SEED

    cmdArgArr(IDX_RECORD_DATA_KV)     % charValue_ptr => PROG_REC_FLAG
    cmdArgArr(IDX_PARAM_FILE_PATH_KV) % charValue_ptr => FILE_PARAM_LIST
    cmdArgArr(IDX_OUT_FILE_PATH_KV)   % charValue_ptr => PROG_OUT_FILE_NAME

    ! Set all initialized commands in this routine.
    do i = lbound(cmdArgArr, 1), ubound(cmdArgArr, 1)
      call pennaCmdArgs%setCmd(     &
          cmdArgArr(i)%cmdName,     &
          cmdArgArr(i)%cmdType,     &
          cmdArgArr(i)%usageTxt,    &
          cmdArgArr(i)%cmdAlias,    &
          cmdArgArr(i)%cmdAliasType &
        )
    end do
  end subroutine initCmdArgRecs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setParams
  !>  Assign parameters from external file and command arguments.
  ! -------------------------------------------------------------------------- !
  subroutine setParams()
    type(CmdArgParser), target :: pennaCmdArgs

    ! Initialize the command argument parser.
    pennaCmdArgs = CmdArgParser()
    call initCmdArgRecs(pennaCmdArgs)

    call pennaCmdArgs%readCmdArgs()

    ! Prefix to file name the relative path to the program executable.
    ! The default parameter file is in the same directory as the executable.
    FILE_PARAM_LIST = prefixRelFilePath(FILE_PARAM_LIST)

    ! Check if the help message is to be printed. Printing the help message has
    ! precendence over initialization of program to help first-time users to
    ! troubleshoot problems.
    if (pennaCmdArgs%isFlagToggled("help")) then
      call printHelpAndNotesMsgs(pennaCmdArgs)
    end if

    ! Get the file path for the parameter listing file.
    if (                                                   &
          pennaCmdArgs%hasValue(                         &
              cmdArgArr(IDX_PARAM_FILE_PATH_KV)%cmdName  &
          )                                                &
        ) then

      cmdArgArr(IDX_PARAM_FILE_PATH_KV)%charValue_ptr = &
        pennaCmdArgs%getCmdValue(cmdArgArr(IDX_PARAM_FILE_PATH_KV)%cmdName)
    end if

    ! Read the model parameters from the config files.
    call readDefaultParamVal()

    ! Assign the parameters obtained from command arguments.
    call assignUserProvidedParams(pennaCmdArgs)

    ! Check if the parameters provided by the user from both config file and
    ! command line arguments are valid
    call checkValidParams(pennaCmdArgs)
  end subroutine setParams

  
  ! -------------------------------------------------------------------------- !
  ! FUNCTION: prefixRelFilePath
  !>  Get relative path to `source`. 
  ! -------------------------------------------------------------------------- !
  function prefixRelFilePath(source) result(relPath)
    character(len=*), intent(in) :: source

    character(len=:), allocatable :: relPath
    character(len=MAX_LEN) :: progPath
    integer :: i

    call get_command_argument(number=0, value=progPath)

    allocate(character(len=0) :: relPath)

    do i = len(progPath), 1, -1
      ! Find the last file path delimiter, i.e. the delimeter before the file
      ! name of the executable.
      if (progPath(i:i) == "/") then
        ! Concatenate the relative path to the executable and the replacement
        ! file name.
        relPath = trim(progPath(1:i)) // trim(source)
        return
      end if
    end do

    ! Raise an error if no file path delimiter was found.
    ! NOTE: The delimiter used is `/`. This may differ in other shells.
    call raiseError("'" // trim(progPath) // "' is not a valid path.")
  end function prefixRelFilePath


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignUserProvidedParams
  !>  Assign the values obtained from command arguments to parameters.
  ! -------------------------------------------------------------------------- !
  subroutine assignUserProvidedParams(pennaCmdArgs)
    type(CmdArgParser), target, intent(inout) :: pennaCmdArgs
      !! Command argument parser for this program.
    type(CmdArgRecord) :: currCmdArg
    integer :: i, castStat

    do i = lbound(cmdArgArr, 1), ubound(cmdArgArr, 1)
      currCmdArg = cmdArgArr(i)

      ! Handle key-value commands.
      if (currCmdArg%cmdType == KV_L .or. currCmdArg%cmdType == KV_S) then
        if (associated(currCmdArg%intValue_ptr)) then
          ! Check if the current key-value command has a value.
          if (pennaCmdArgs%hasValue(currCmdArg%cmdName)) then
            ! Get value and cast it to integer.
            currCmdArg%intValue_ptr = castCharToInt( &
              pennaCmdArgs%getCmdValue(currCmdArg%cmdName), castStat &
            )
            ! Check casting status.
            if (castStat /= 0) then
              ! NOTE: Recall that `cmdName`s are short commands while
              ! `cmdAlias`s are long ones.
              call raiseError( &
                "Invalid value for '-" // currCmdArg%cmdName // &
                "' or '--" // currCmdArg%cmdAlias // "'. Must be integer." &
                )
            end if
          end if
        
        else if (associated(currCmdArg%realValue_ptr)) then
          ! Check if the current key-value command has a value.
          if (pennaCmdArgs%hasValue(currCmdArg%cmdName)) then
            ! Get the value and cast it to real
            currCmdArg%realValue_ptr = castCharToReal( &
              pennaCmdArgs%getCmdValue(currCmdArg%cmdName), castStat &
            )

            ! Check casting status
            if (castStat /= 0) then
              call raiseError( &
                "Invalid value for '-" // currCmdArg%cmdName // &
                "' or '--" // currCmdArg%cmdAlias // "'. Must be real." &
              )
            end if
          end if

        else if (associated(currCmdArg%charValue_ptr)) then
          ! Check if the current key-value command has a value.
          if (pennaCmdArgs%hasValue(currCmdArg%cmdName)) then
            currCmdArg%charValue_ptr = &
              pennaCmdArgs%getCmdValue(currCmdArg%cmdName)
          end if

        else
          call raiseError( &
            "Internal error encountered. " // &
            "'intValue_ptr' and 'charValue_ptr' must not be associated at " // &
            "the same time" &
          )
        end if

      ! Handle flag commands.
      else

        ! === Verbose print flag ============================================= !
        if (                                       &
            currCmdArg%cmdName ==                  &
              cmdArgArr(IDX_SHOW_PARAM_F)%cmdName  &
            ) then
          if (                                         &
                pennaCmdArgs%isFlagToggled(            &
                  cmdArgArr(IDX_SHOW_PARAM_F)%cmdName  &
                )                                      &
              ) then
            PROG_PRINT_STATE = VERBOSE_PRINT
          end if

          ! === Silent print flag ============================================ !
        else if (                                  &
              currCmdArg%cmdName ==                &
                cmdArgArr(IDX_NO_PARAM_F)%cmdName  &
            ) then
          if (                                       &
                pennaCmdArgs%isFlagToggled(          &
                  cmdArgArr(IDX_NO_PARAM_F)%cmdName  &
                )                                    &
              ) then
            PROG_PRINT_STATE = SILENT_PRINT
          end if

        ! === Print version flag ============================================= !
        else if (                                    &
              currCmdArg%cmdName ==                  &
              cmdArgArr(IDX_SHOW_VERSION_F)%cmdName  &
            ) then
          if (                                            &
                pennaCmdArgs%isFlagToggled(               &
                  cmdArgArr(IDX_SHOW_VERSION_F)%cmdName   &
                )                                         &
              ) then
            PROG_PRINT_STATE = VERSION_PRINT
          end if

        ! === CSV Format flag ================================================ !
        else if (                                    &
              currCmdArg%cmdName ==                  &
              cmdArgArr(IDX_CSV_FORMAT_F)%cmdName  &
            ) then
          PROG_IN_CSV_FMT = (                          &
              pennaCmdArgs%isFlagToggled(              &
                cmdArgArr(IDX_CSV_FORMAT_F)%cmdName  &
              )                                        &
          )

        else
          call raiseError( &
              "Unknown flag command '" //  currCmdArg%cmdName // "'." &
            )
        end  if
      end if
    end do
  end subroutine assignUserProvidedParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkValidParams
  !>  Check the assigned model parameters for invalid values.
  ! -------------------------------------------------------------------------- !
  subroutine checkValidParams(pennaCmdArgs)
    type(CmdArgParser), target, intent(inout) :: pennaCmdArgs
    type(CmdArgRecord) :: cmdArg

    logical :: printNoParam, printAllParam
    integer :: i

    printNoParam = (                           &
          pennaCmdArgs%isFlagToggled(          &
            cmdArgArr(IDX_NO_PARAM_F)%cmdName  &
          )                                    &
      )
    printAllParam = (                                                        &
          pennaCmdArgs%isFlagToggled(cmdArgArr(IDX_SHOW_PARAM_F)%cmdName)  &
      )

    ! Check invalid combination of toggle options.
    if (printNoParam .and. printAllParam) then
      call raiseError( &
        "Flag options for verbose print and silent print cannot be chosen " // &
        "together." &
      )
    end if

    ! Check for invalid positive integer arguments.
    do i = lbound(CMD_POSITIVE_INT, 1), ubound(CMD_POSITIVE_INT, 1)
      cmdArg = cmdArgArr(CMD_POSITIVE_INT(i))
      if (cmdArg%intValue_ptr <= 0) then
        call raiseError(                                       &
            "Invalid value for '" // trim(cmdArg%usageTxt ) // &
            "' (="// castIntToChar(cmdArg%intValue_ptr) //     &
            "). Must be a positive integer."                   &
          )
      end if
    end do

    
    if (cmdArgArr(IDX_INIT_MTTN_COUNT_KV)%intValue_ptr < -1) then
      cmdArg = cmdArgArr(IDX_INIT_MTTN_COUNT_KV)
      call raiseError(                                     &
          "Invalid value for " //                          &
          "'Initial mutation count per individual'. (=" // &
          castIntToChar(cmdArg%intValue_ptr)            // &
          "Must be either a non-negative integer or "   // &
          "-1 (for the fully random case)"                 &
        )
    end if
  end subroutine checkValidParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printParams
  !>  Pretty print the model parameters. Can print verbosely or print nothing
  !!  if need be.
  ! -------------------------------------------------------------------------- !
  subroutine printProgDetails()
    select case (PROG_PRINT_STATE)
      case (NORMAL_PRINT, VERBOSE_PRINT)
        call printParams()

      case (VERSION_PRINT)
        call printVersion()
      
      case (SILENT_PRINT)
        ! Do nothing

      case default
        call raiseError("Invalid 'PROG_PRINT_STATE' value.")
      end select
  end subroutine printProgDetails


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printHelpAndNotesMsgs
  !>  Print the help messages including some notes.
  ! -------------------------------------------------------------------------- !
  subroutine printHelpAndNotesMsgs(pennaCmdArgs)
    type(CmdArgParser), target, intent(inout) :: pennaCmdArgs

    character(len=:), allocatable :: progName

    ! Initialize local variables.
    allocate(character(len=0) :: progName)
    progName = getProgName()

    call pennaCmdArgs%printHelp(progName, PROG_DESC)

    ! Print the additional notes.
    print "(/a)", "NOTES:"
    print "(' - ', 5(a)/, '  ', 2(a)/)",                                     &
      "Negative values for initial mutation count (-",                       &
      cmdArgArr(IDX_INIT_MTTN_COUNT_KV)%cmdName, " / --",                    &
      cmdArgArr(IDX_INIT_MTTN_COUNT_KV)%cmdAlias, ")",                       &
      " are interpreted as random initial mutation count for each of the ",  &
      "individuals."
    print "(' - ', *(a))",                                     &
      "Valid character values for -", cmdArgArr(IDX_RECORD_DATA_KV)%cmdName, &
      " or --", cmdArgArr(IDX_RECORD_DATA_KV)%cmdAlias
    write(*, "(8(4(' '), a/)/)", advance="no")                                 &
      "x - Record nothing.",                                                   &
      "p - Population size per time step",                                     &
      "a - Age distribution in the final time steps.",                         &
      "d - Death count per time step.",                                        &
      "s - Genetic diversity index per time step (Normalized Shannon index).", &
      "b - Bad gene distribution per time step.",                              &
      "t - (Average) elapsed time and its std deviation if applicable",        &
      "c - Number of unique genome counts per time step."

    print "(' - ', *(a))",  &
      "Valid real values for -", cmdArgArr(IDX_ENTROPY_ORDER_KV)%cmdName, &
      " or --", cmdArgArr(IDX_ENTROPY_ORDER_KV)%cmdAlias
    write(*, "(3(4(' '), a/)/)", advance="no")  &
      "NaN/infinity (default) - Normalized Shannon entropy", &
      "1.0                    - Shannon entropy",            &
      "Other reals            - Renyi entropy"

    print "(' - ', *(a))",  &
      "Valid integer values for -", cmdArgArr(IDX_RNG_CHOICE_KV)%cmdName, &
      " or --", cmdArgArr(IDX_RNG_CHOICE_KV)%cmdAlias
    write(*, "(2(4(' '), a/)/)", advance="no")  &
      "0 - Intrinsic RNG by the compiler (" // compiler_version() // ")", &
      "1 - 32-bit Mersenne twister PRNG"

    print "(' - ', *(a))",  &
      "Formatting for the output file name -", &
      cmdArgArr(IDX_OUT_FILE_PATH_KV)%cmdName, &
      " or --", cmdArgArr(IDX_OUT_FILE_PATH_KV)%cmdAlias
    write(*, "(4(4(' '), a/)/)", advance="no")  &
      "%[N]n - Data set number. Can be optionally left-padded with " //        &
        "N number of zeroes",                                                  &
      "%[N]f - The record data flag provided by -" //                          &
        cmdArgArr(IDX_RECORD_DATA_KV)%cmdName // " or --" //                   &
        cmdArgArr(IDX_RECORD_DATA_KV)%cmdAlias // " option. ",                 &
        "        Can be optionally left-padded with N whitespace characters.", &
      "Note that the brackets denote optionality. " //                         &
      "So '%n' and '%5n' are both valid."

    print "(/' ', *(a))",                                      &
      "Default values of all key-value options above are, " // &
      "by default, listed in '", trim(FILE_PARAM_LIST),        &
      "'.", new_line("")
    stop
  end subroutine printHelpAndNotesMsgs
  

  ! -------------------------------------------------------------------------- !
  ! FUNCTION: getProgName
  !>  Get the name of this program.
  ! -------------------------------------------------------------------------- !
  function getProgName() result(progName)
    character(len=:), allocatable :: progName

    character(len=MAX_LEN) :: filePathExec
    integer :: i

    ! Initialize output.
    allocate(character(len=0) :: progName)

    ! Get the file path of the program relative to the dir this program is run.
    call get_command_argument(0, filePathExec)

    do i = len_trim(filePathExec), 1, -1
      ! Find the last file path delimiter, i.e. the delimeter before the file
      ! name of the executable.
      if (filePathExec(i:i) == "/") then
        exit
      else
        progName = filePathExec(i:i) // progName
      end if
    end do
  end function getProgName


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printParams
  !>  Pretty print the model and program parameters.
  ! -------------------------------------------------------------------------- !
  subroutine printParams()
    ! Pretty print separator.
    integer :: k
    character, parameter :: PRINT_SEPARATOR(*) = [("=", k = 1, 29)]

    ! ***Header
    print "(*(a))", PRINT_SEPARATOR 
    print "(a)", "Asexual Penna model"
    print "(*(a))", PRINT_SEPARATOR

    ! ***Model and program parameters.
    if (PROG_PRINT_STATE == VERBOSE_PRINT) then
      write(*, "(*(a20, i9/))", advance="no") &
          "Genome length",        MODEL_L, &
          "Mutation threshold",   MODEL_T, &
          "Birth rate",           MODEL_B, &
          "Mutation rate",        MODEL_M, &
          "Min reproduction age", MODEL_R, &
          "Max reproduction age", MODEL_R_MAX, &
          "Carrying capacity",    MODEL_K, &
          "Number of time steps", MODEL_TIME_STEPS, &
          "Init mutation count",  MODEL_MTTN_COUNT, &
          "Sample size",          PROG_SAMPLE_SIZE, &
          "Starting pop size",    MODEL_START_POP_SIZE

      write(*, "(*(a20, a))", advance="no") &
          "Record flag", repeat(" ", 9 - len(REC_FLAG_ORDER))
      do k = 1, len(REC_FLAG_ORDER)
        if (scan(PROG_REC_FLAG, REC_FLAG_ORDER(k:k)) > 0) then
          write(*, "(a)", advance="no") REC_FLAG_ORDER(k:k)
        else
          write(*, "(a)", advance="no") "-"
        end if
      end do
      write(*, "(a)")
      
      write(*, "(a20)", advance="no") "Renyi entropy order"
      if (isFinite(MODEL_ENTROPY_ORDER)) then
        write(*, "(f9.2/)", advance="no") MODEL_ENTROPY_ORDER
      else
        write(*, "(f9.2/, a29/)", advance="no") 1.0, "(Normalized)"
      end if

      print "(*(a))", PRINT_SEPARATOR
    end if
  end subroutine printParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printVersion
  !>  Print the program version and stop the program.
  ! -------------------------------------------------------------------------- !
  subroutine printVersion()
    print "(a, ' ', a)", trim(PROG_NAME), trim(PROG_VERSION)
    stop
  end subroutine printVersion


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeParamAlloctbls
  !>  Free all allocatable objects and objects with allocatable attributes in
  !!  the 'Parameter' module.
  ! -------------------------------------------------------------------------- !
  subroutine freeParamAlloctbls()
    integer :: i

    ! Free Verhulst weight array.
    if (allocated(MODEL_V_WEIGHT)) deallocate(MODEL_V_WEIGHT)

    ! Free all allocatable attributes of all `CmdArgRecord` objects.
    do i = lbound(cmdArgArr, 1), ubound(cmdArgArr, 1)
      ! Free allocatable character attributes.
      if (allocated(cmdArgArr(i)%cmdName)) deallocate(cmdArgArr(i)%cmdName)
      if (allocated(cmdArgArr(i)%cmdAlias)) deallocate(cmdArgArr(i)%cmdAlias)
      if (allocated(cmdArgArr(i)%usageTxt)) deallocate(cmdArgArr(i)%usageTxt)

      ! Nullify pointer attributes.
      cmdArgArr(i)%charValue_ptr => null()
      cmdArgArr(i)%intValue_ptr => null()
    end do
  end subroutine freeParamAlloctbls
end submodule CmdArgAssignProcs

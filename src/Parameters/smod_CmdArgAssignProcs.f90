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

  type :: CmdArgRecord_t
    !! A handy derived type for collecting command option attributes.
    character(len=:), allocatable :: cmdName
      !! Name of the command.
    character(len=:), allocatable :: cmdAlias
      !! Alias for `cmdName`.
    character(len=:), allocatable :: cmdGroup
      !! Command group.
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
    real(kind=real32),      pointer :: real32Value_ptr => null()
      !! Pointer to the real parameter of default kind `cmdName` modifies
    real(kind=real64),      pointer :: real64Value_ptr => null()
      !! Pointer to the real64 parameter `cmdName` modifies
  end type

  ! Command line arguments
  ! -------------------------------------------------------------------------- !
  ! Array of all commands
  type(CmdArgRecord_t) :: cmdArgArr(23)

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
  integer, parameter :: IDX_TMDP_PARAM_KV      = 12
  integer, parameter :: IDX_TMDP_PARAM_DT_KV   = 13
  
  ! Program parameters.
  integer, parameter :: IDX_SAMPLE_SIZE_KV     = 14
  integer, parameter :: IDX_RECORD_DATA_KV     = 15  ! String-valued command
  integer, parameter :: IDX_RNG_CHOICE_KV      = 16  ! Special constraint value
  integer, parameter :: IDX_RNG_SEED_KV        = 17  ! Any integer
  integer, parameter :: IDX_PARAM_FILE_PATH_KV = 18  ! String-valued command
  integer, parameter :: IDX_OUT_FILE_PATH_KV   = 19  ! String-valued command
  integer, parameter :: IDX_SHOW_PARAM_F       = 20
  integer, parameter :: IDX_NO_PARAM_F         = 21
  integer, parameter :: IDX_SHOW_VERSION_F     = 22
  integer, parameter :: IDX_OUT_FMT_KV         = 23

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
    IDX_SAMPLE_SIZE_KV,    &
    IDX_TMDP_PARAM_DT_KV   &
  ]

  integer, parameter :: CMD_CASE_INSENSITIVE(*) = [ &
    IDX_RECORD_DATA_KV,  &
    IDX_TMDP_PARAM_KV,   &
    IDX_OUT_FMT_KV       &
  ]

  ! Command group names
  character(len=*), parameter :: CMD_GROUP_PENNA = "Penna model"
  character(len=*), parameter :: CMD_GROUP_PROG  = "Program parameter"
  character(len=*), parameter :: CMD_GROUP_REC   = "Data recording"
  character(len=*), parameter :: CMD_GROUP_MISC  = achar(0)
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initCmdArgRecs
  !>  Initialize `CmdArgRecord_t` objects in the `cmdArgArr` array.
  ! -------------------------------------------------------------------------- !
  subroutine initCmdArgRecs(pennaCmdArgs)
    type(CmdArgParser_t), target, intent(inout) :: pennaCmdArgs
      !! Command argument parser for this program.
    integer :: i

    ! Model parameters.
    cmdArgArr(IDX_MTTN_THRESHOLD_KV) = CmdArgRecord_t(  &
        "T", "mttn-lim", CMD_GROUP_PENNA, KV_S, KV_L,   &
        "Mutation threshold"                            &
      )
    cmdArgArr(IDX_BIRTH_RATE_KV) = CmdArgRecord_t(       &
        "B", "birth-rate", CMD_GROUP_PENNA, KV_S, KV_L,  &
        "Birth rate"                                     &
      )
    cmdArgArr(IDX_MTTN_RATE_KV) = CmdArgRecord_t(       &
        "M", "mttn-rate", CMD_GROUP_PENNA, KV_S, KV_L,  &
        "Mutation rate"                                 &
      )
    cmdArgArr(IDX_R_AGE_MIN_KV) = CmdArgRecord_t(      &
        "r", "r-age-min", CMD_GROUP_PENNA, KV_S, KV_L, &
        "Minimum reproduction age"                     &
      )
    cmdArgArr(IDX_R_AGE_MAX_KV) = CmdArgRecord_t(       &
        "R", "r-age-max", CMD_GROUP_PENNA, KV_S, KV_L,  &
        "Maximum reproduction age"                      &
      )
    cmdArgArr(IDX_MAX_POP_SIZE_KV) = CmdArgRecord_t(  &
        "K", "pop-cap", CMD_GROUP_PENNA, KV_S, KV_L,  &
        "Carrying capacity"                           &
      )
    cmdArgArr(IDX_INIT_MTTN_COUNT_KV) = CmdArgRecord_t(  &
        "m", "init-mttn", CMD_GROUP_PENNA, KV_S, KV_L,   &
        "Initial mutation count per individual"          &
      )
    cmdArgArr(IDX_INIT_POP_SIZE_KV) = CmdArgRecord_t(  &
        "p", "pop-size", CMD_GROUP_PENNA, KV_S, KV_L,  &
        "Initial population size"                      &
      )
    cmdArgArr(IDX_MAX_TIME_STEP_KV) = CmdArgRecord_t(  &
        "t", "time-step", CMD_GROUP_PENNA, KV_S, KV_L, &
        "Maximum time step"                            &
      )
    cmdArgArr(IDX_ENTROPY_ORDER_KV) = CmdArgRecord_t(  &
        "O", "ent-order", CMD_GROUP_REC, KV_S, KV_L,   &
        "Renyi entropy order"  &
      )
    cmdArgArr(IDX_AGE_DSTRB_TIME_KV) = CmdArgRecord_t(        &
        "a", "age-dstrb-time", CMD_GROUP_REC, KV_S, KV_L,     &
        "Age distribution time step till the final time step" &
      )
    cmdArgArr(IDX_TMDP_PARAM_KV) = CmdArgRecord_t(                          &
        "P", "tmdp-param", CMD_GROUP_PENNA, KV_S, KV_L,                     &
        "Time-dependent model parameter. See 'NOTES' for parameter choices" &
      )
    cmdArgArr(IDX_TMDP_PARAM_DT_KV) = CmdArgRecord_t(                          &
        "Q", "tmdp-param-dt", CMD_GROUP_PENNA, KV_S, KV_L,                     &
        "Time period between increments of the time-dependent model parameter" &
      )

    ! Program parameters.
    cmdArgArr(IDX_SAMPLE_SIZE_KV) = CmdArgRecord_t(      &
        "s", "sample-size", CMD_GROUP_PROG, KV_S, KV_L,  &
        "Sample size"                                    &
      )
    cmdArgArr(IDX_RECORD_DATA_KV) = CmdArgRecord_t(                    &
        "d", "record-data", CMD_GROUP_REC, KV_S, KV_L,                 &
        "Record data. See 'NOTES' below for the recordable data sets"  &
      )
    cmdArgArr(IDX_RNG_CHOICE_KV) = CmdArgRecord_t(              &
        "g", "rng-choice", CMD_GROUP_PROG, KV_S, KV_L,          &
        "RNG choice. See 'NOTES' below for the available RNGs"  &
      )
    cmdArgArr(IDX_RNG_SEED_KV) = CmdArgRecord_t(      &
        "S", "rng-seed", CMD_GROUP_PROG, KV_S, KV_L,  &
        "RNG seed. Must be integer"                   &
      )
    cmdArgArr(IDX_PARAM_FILE_PATH_KV) = CmdArgRecord_t(              &
        "f", "param-file", CMD_GROUP_PROG, KV_S, KV_L,               &
        "Path to the file containing the default parameter listing"  &
      )
    cmdArgArr(IDX_OUT_FILE_PATH_KV) = CmdArgRecord_t(   &
        "o", "out", CMD_GROUP_REC, KV_S, KV_L,          &
        "Path to the file the data is to be written in" &
      )
    cmdArgArr(IDX_SHOW_PARAM_F) = CmdArgRecord_t(        &
        "v", "verbose", CMD_GROUP_MISC, FLAG_S, FLAG_L,  &
        "Show more information about the current run"    &
      )
    cmdArgArr(IDX_NO_PARAM_F) = CmdArgRecord_t(               &
        "q", "quiet", CMD_GROUP_MISC, FLAG_S, FLAG_L,         &
        "Show no output on STDOUT when running this program"  &
      )
    cmdArgArr(IDX_SHOW_VERSION_F) = CmdArgRecord_t(      &
        "V", "version", CMD_GROUP_MISC, FLAG_S, FLAG_L,  &
        "Show the version of this program"               &
      )
    cmdArgArr(IDX_OUT_FMT_KV) = CmdArgRecord_t(              &
        "F", "out-format", CMD_GROUP_REC, KV_S, KV_L,        &
        "Output file format. See 'NOTES' below for choices." &
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
    cmdArgArr(IDX_ENTROPY_ORDER_KV)   % real64Value_ptr => MODEL_ENTROPY_ORDER
    cmdArgArr(IDX_AGE_DSTRB_TIME_KV)  % intValue_ptr  &
      => MODEL_AGE_DSTRB_INIT_TIMESTEP
    cmdArgArr(IDX_TMDP_PARAM_KV)      % charValue_ptr &
      => MODEL_TIME_DEPENDENT_PARAM
    cmdArgArr(IDX_TMDP_PARAM_DT_KV)   % intValue_ptr => MODEL_TMDP_PARAM_DELTA_T

    cmdArgArr(IDX_SAMPLE_SIZE_KV) % intValue_ptr => PROG_SAMPLE_SIZE
    cmdArgArr(IDX_RNG_CHOICE_KV)  % intValue_ptr => PROG_RNG
    cmdArgArr(IDX_RNG_SEED_KV)    % intValue_ptr => PROG_RNG_SEED

    cmdArgArr(IDX_RECORD_DATA_KV)     % charValue_ptr => PROG_REC_FLAG
    cmdArgArr(IDX_PARAM_FILE_PATH_KV) % charValue_ptr => FILE_PARAM_LIST
    cmdArgArr(IDX_OUT_FILE_PATH_KV)   % charValue_ptr => PROG_OUT_FILE_NAME
    cmdArgArr(IDX_OUT_FMT_KV)         % charValue_ptr => PROG_OUT_FMT

    ! Set all initialized commands in this routine.
    do i = lbound(cmdArgArr, 1), ubound(cmdArgArr, 1)
      call pennaCmdArgs%setCmd(      &
          cmdArgArr(i)%cmdName,      &
          cmdArgArr(i)%cmdType,      &
          cmdArgArr(i)%usageTxt,     &
          cmdArgArr(i)%cmdAlias,     &
          cmdArgArr(i)%cmdAliasType, &
          cmdArgArr(i)%cmdGroup      &
        )
    end do
  end subroutine initCmdArgRecs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setParams
  !>  Assign parameters from external file and command arguments.
  ! -------------------------------------------------------------------------- !
  module subroutine setParams()
    type(CmdArgParser_t), target :: pennaCmdArgs

    ! Initialize the command argument parser.
    call init_CmdArgParser(pennaCmdArgs)
    call initCmdArgRecs(pennaCmdArgs)

    call pennaCmdArgs%readCmdArgs()

    ! Prefix to file name the relative path to the program executable.
    ! The default parameter file is in the same directory as the executable.
    FILE_PARAM_LIST = prefixRelFilePath(FILE_PARAM_LIST)

    ! Get the file path for the parameter listing file.
    if (pennaCmdArgs%hasValue(cmdArgArr(IDX_PARAM_FILE_PATH_KV)%cmdName)) then
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

    ! Check if the help message is to be printed.
    if (pennaCmdArgs%isFlagToggled("help")) then
      call printHelpAndNotesMsgs(pennaCmdArgs)
    end if
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
    type(CmdArgParser_t), target, intent(inout) :: pennaCmdArgs
      !! Command argument parser for this program.
    type(CmdArgRecord_t) :: currCmdArg
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
        
        else if (associated(currCmdArg%real32Value_ptr)) then
          ! Check if the current key-value command has a value.
          if (pennaCmdArgs%hasValue(currCmdArg%cmdName)) then
            ! Get the value and cast it to real
            currCmdArg%real32Value_ptr = castCharToReal32( &
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

        else if (associated(currCmdArg%real64Value_ptr)) then
          ! Check if the current key-value command has a value.
          if (pennaCmdArgs%hasValue(currCmdArg%cmdName)) then
            ! Get the value and cast it to real
            currCmdArg%real64Value_ptr = castCharToReal64( &
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
          ! By convention, no-case -> lowercase
          if (any(i == CMD_CASE_INSENSITIVE)) then
            currCmdArg%charValue_ptr = toLower(currCmdArg%charValue_ptr)
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

        else
          call raiseError( &
              "Unknown flag command '" //  currCmdArg%cmdName // "'." &
            )
        end  if
      end if

      ! Assign special values.
      if (i == IDX_TMDP_PARAM_KV) then
        call setTimeDependentParam(                          &
            cmdArgArr(IDX_TMDP_PARAM_KV)%charValue_ptr(1:1)  &
        )
      end if
    end do
  end subroutine assignUserProvidedParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setTimeDependentParam
  !>  Assign the pointer for the time-dependent Penna model parameter.
  ! -------------------------------------------------------------------------- !
  subroutine setTimeDependentParam(userChoice)
    character, intent(in) :: userChoice

    select case (userChoice)
    case (TMDP_PARAM_BIRTH)
      MODEL_TIME_DEPENDENT_PARAM_PTR => MODEL_B
    case (TMDP_PARAM_MTTN_RATE)
      MODEL_TIME_DEPENDENT_PARAM_PTR => MODEL_M
    case (TMDP_PARAM_R_AGE)
      MODEL_TIME_DEPENDENT_PARAM_PTR => MODEL_R
    case (TMDP_PARAM_MTTN_LIM)
      MODEL_TIME_DEPENDENT_PARAM_PTR => MODEL_T
    case (TMDP_PARAM_NULL)
      MODEL_TIME_DEPENDENT_PARAM_PTR => null()   ! Do nothing
    case default
      call raiseError(  &
        "Invalid choice for a time-dependent Penna model parameter: " // &
        userChoice // ""  &
      )
    end select
  end subroutine setTimeDependentParam


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: incrementTimeDependentParam
  !>  Increment the time-dependent parameter if the argument `timeStep` is
  !!  a multiple of `MODEL_TMDP_PARAM_DELTA_T`.
  !!  If the optional argument `reset` is true, reset the parameter to its
  !!  starting value. It is FALSE by default.
  ! -------------------------------------------------------------------------- !
  module subroutine incrementTimeDependentParam(timeStep, reset)
    integer, intent(in)           :: timeStep
      !! Time step supplied by the Penna model simulation.
    logical, intent(in), optional :: reset
      !! Reset the time-dependent parameter if present and true.

    integer, save :: startingValue = -1

    ! Set the starting value.
    if (startingValue == -1) then
      startingValue = MODEL_TIME_DEPENDENT_PARAM_PTR
    end if

    ! Check for reset.
    if (present(reset)) then
      if (reset) then
        MODEL_TIME_DEPENDENT_PARAM_PTR = startingValue

        ! We make R_max change with R.
        if (associated(MODEL_TIME_DEPENDENT_PARAM_PTR, MODEL_R)) then
          MODEL_R_MAX = startingValue
        end if
        return
      end if
    end if

    ! Check if the time-dependent is set to change.
    if (modulo(timeStep, MODEL_TMDP_PARAM_DELTA_T) /= 0) return

    if (associated(MODEL_TIME_DEPENDENT_PARAM_PTR)) then
      MODEL_TIME_DEPENDENT_PARAM_PTR = MODEL_TIME_DEPENDENT_PARAM_PTR + 1

      ! We make R_max change with R.
      if (associated(MODEL_TIME_DEPENDENT_PARAM_PTR, MODEL_R)) then
        MODEL_R_MAX = MODEL_R_MAX + 1
      end if
    end if
  end subroutine incrementTimeDependentParam


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkValidParams
  !>  Check the assigned model parameters for invalid values.
  ! -------------------------------------------------------------------------- !
  subroutine checkValidParams(pennaCmdArgs)
    type(CmdArgParser_t), target, intent(inout) :: pennaCmdArgs
    type(CmdArgRecord_t) :: cmdArg

    logical :: printNoParam, printAllParam
    logical :: cmpRNGchoices(size(RNG_ALL_CHOICES))
    logical :: isValidFileFmt
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
        call raiseError(                                            &
            "Invalid value for '" // trim(cmdArg%usageTxt)       // &
            "' (" // castIntToChar(cmdArg%intValue_ptr) // "). " // &
            "Must be a positive integer."                           &
          )
      end if
    end do

    ! Check for invalid Renyi entropy order
    ! (must be non-negative or non-finite)
    if (isFinite64(MODEL_ENTROPY_ORDER)) then
      if (MODEL_ENTROPY_ORDER < 0.0_real64) then
        call raiseError( &
          "Invalid value for the Renyi entropy order " //            &
          "(" // castReal64ToChar(MODEL_ENTROPY_ORDER)   // "). " // &
          "Must be either non-negative or non-finite " //            &
          "(default, Normalized Shannon)." &
        )
      end if
    end if

    ! Check for valid initial mutation input (>= -1)
    if (MODEL_MTTN_COUNT < 0 .and. MODEL_MTTN_COUNT /= MTTN_COUNT_RANDOM) then
      call raiseError(                                       &
          "Invalid value for the initial mutation count " // &
          "(" // castIntToChar(MODEL_MTTN_COUNT) // ")."  // &
          new_line(":D")                                  // &
          "Must be either a non-negative integer or "     // &
          castIntToChar(MTTN_COUNT_RANDOM)                // &
          " for the fully random case."                      &
        )
    end if

    ! Check for valid RNG choice
    cmpRNGchoices = (RNG_ALL_CHOICES(:) == PROG_RNG)
    if (.not.any(cmpRNGchoices)) then
      call raiseError(  &
          "Invalid RNG choice (" // castIntToChar(PROG_RNG) // "). " // &
          "See the HELP message for the valid RNGs."                    &
      )
    end if

    ! Check for valid output file choices
    isValidFileFmt = .false.
    do i = lbound(OUT_FMT_CHOICES, 1), ubound(OUT_FMT_CHOICES, 1)
      isValidFileFmt = (trim(OUT_FMT_CHOICES(i)) == trim(PROG_OUT_FMT))
      if (isValidFileFmt) exit
    end do
    if (.not. isValidFileFmt) then
      call raiseError(  &
        "Invalid output format ('" // trim(PROG_OUT_FMT) // "'). " // &
        "See the HELP message for valid output file formats."   &
      )
    end if
  end subroutine checkValidParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printParams
  !>  Pretty print the model parameters. Can print verbosely or print nothing
  !!  if need be.
  ! -------------------------------------------------------------------------- !
  module subroutine printProgDetails()
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
    type(CmdArgParser_t), target, intent(inout) :: pennaCmdArgs

    character(len=:), allocatable :: progName

    ! Initialize local variables.
    allocate(character(len=0) :: progName)
    progName = getProgName()

    call pennaCmdArgs%printHelp(progName, PROG_DESC)

    ! Print the additional notes.
    print "(//40x, a)", "NOTES"
    print "(40x, a)",  "-----"

    ! Initial mutation values.
    print "(' - ', 5(a)/, 2x, 2(a)/)",                                       &
      "Negative values for initial mutation count (-",                       &
      cmdArgArr(IDX_INIT_MTTN_COUNT_KV)%cmdName, " or --",                   &
      cmdArgArr(IDX_INIT_MTTN_COUNT_KV)%cmdAlias, ")",                       &
      " are interpreted as random initial mutation count for each of the ",  &
      "individuals."

    ! Data record flags.
    print "(' - ', *(a))", &
      "Valid flags for time-dependent model parameter (-", &
      cmdArgArr(IDX_TMDP_PARAM_KV)%cmdName, " or --",      &
      cmdArgArr(IDX_TMDP_PARAM_KV)%cmdAlias, "):"
    write(*, "(5(5x, a1, ' - ', a/))", advance="no")  &
      TMDP_PARAM_NULL,      "No time-dependent parameter.",             &
      TMDP_PARAM_BIRTH,     "Birth rate.",                              &
      TMDP_PARAM_MTTN_RATE, "Mutation rate.",                           &
      TMDP_PARAM_MTTN_LIM,  "Mutation threshold",                       &
      TMDP_PARAM_R_AGE,     "Minimum and maximum reproduction age. " // &
          "Note that both of them increment"
    print "( (3x, a/) )", &
      "One can choose only one parameter to vary with time. " //  &
      "The flags are also case-insensitive."

    ! Data record flags.
    print "(' - ', *(a))",                             &
      "Valid character values for record flags (-",    &
      cmdArgArr(IDX_RECORD_DATA_KV)%cmdName, " or --", &
      cmdArgArr(IDX_RECORD_DATA_KV)%cmdAlias, "):"
    write(*, "(8(5x, a1, ' - ', a/))", advance="no")               &
      REC_NULL,       "Record nothing.",                           &
      REC_POP,        "Population size per time step",             &
      REC_AGE_DSTRB,  "Age distribution in the final time steps.", &
      REC_DEATH,      "Death count per time step.",                &
      REC_DIV_IDX,                                                 &
        "(Renyi) entropy as genetic diversity per time step.",     &
      REC_GENE_DSTRB, "Bad gene distribution per time step.",      &
      REC_TIME,       "Mean elapsed time and std dev",             &
      REC_GNM_COUNT,  "Number of unique genome counts per time step."
    print "( 2(3x, a/) )",                                                    &
      "The flags are case-insensitive. Multiple flags can also be on " //     &
        "simultaneously.", &
      "e.g. 'psb' for population size, entropy and bad gene distribution " // &
          "per time."

    ! Valid Renyi entropy order values.
    print "(' - ', *(a))",                               &
      "Valid real values for Renyi entropy order (-",    &
      cmdArgArr(IDX_ENTROPY_ORDER_KV)%cmdName, " or --", &
      cmdArgArr(IDX_ENTROPY_ORDER_KV)%cmdAlias, "):"
    write(*, "(3(5x, a/))", advance="no")                    &
      "NaN/infinity (default) - Normalized Shannon entropy", &
      "1.0                    - Shannon entropy",            &
      "Other reals            - Renyi entropy"
    print "(3x, a)", ""   ! No additional notes

    ! RNG choices.
    print "(' - ', *(a))",                            &
      "Valid integer values for RNG choices (-",      &
      cmdArgArr(IDX_RNG_CHOICE_KV)%cmdName, " or --", &
      cmdArgArr(IDX_RNG_CHOICE_KV)%cmdAlias, "):"
    print "(5x, i1, ' - ', a/, 10x, a)",               &
      RNG_INTRINSIC, "Intrinsic RNG by the compiler:", &
          "(" // compiler_version() // ")"
    print "(5x, i1, ' - ', a)",  &
      RNG_MERSENNE_TWISTER, "32-bit Mersenne twister PRNG"
    print "(3x, a)", ""   ! No additional notes
    
    ! Output file formats.
    print "(' - ', *(a))",                          &
      "Valid string values for output format (-",   &
      cmdArgArr(IDX_OUT_FMT_KV)%cmdName, " or --",  &
      cmdArgArr(IDX_OUT_FMT_KV)%cmdAlias, "):"
    write(*, "(3(5x, a6, ' - ', a/))", advance="no") &
      OUT_FMT_READABLE, "Human-readable format",     &
      OUT_FMT_CSV,      "CSV format",                &
      OUT_FMT_BINARY,   "Binary format"
    print "( 2(3x, a/), 4(5x, a/), 4(3x, a/))", &
      "In binary-formatted output files, the initial 16 bytes contain " // &
        "the header. ",                                                    &
      "It is divided into 4, 4 bytes each, which represent the following:", &
        "1. The size of each data points in bytes",         &
        "2. The number of data points in each row of data", &
        "3. The type of data the file contains",            &
        "4. Padding as protection from buffer overwrite",   &
      "The data proper in the binary-formatted files consists of rows of " // &
        "data, internally represented as 'records'.",                        &
      "The size of each rows is obtained by multiplying header " // &
        "values (1) and (2).",                                      &
      "The counting of records start from the beginning of the file, " // &
        "such that the header is within the first record(s).", &
      "All data in this format is little-endian."

    ! Output file name formatting.
    print "(' - ', *(a))",                      &
      "Formatting for the output file name (-", &
      cmdArgArr(IDX_OUT_FILE_PATH_KV)%cmdName,  " or --", &
      cmdArgArr(IDX_OUT_FILE_PATH_KV)%cmdAlias, "):"
    write(*, "(2(5x, a/))", advance="no")                                      &
      "%[N]n - Data set number for multiple samples.",                         &
      "%[N]f - The record data flag provided by -" //                          &
        cmdArgArr(IDX_RECORD_DATA_KV)%cmdName // " or --" //                   &
        cmdArgArr(IDX_RECORD_DATA_KV)%cmdAlias // " option. "
    print "(3(3x, a/))",                                                   &
      "`N` is the number of characters padded to the left of the data " // &
          "specified by the formatting.",                                  &
      "It is optional and defaults to 0, e.g. %5f and %f are valid. ",     &
      "The padding characters are '_' for %f and '0' for %n."

    print "(1x, a/)",                                                         &
      "By default, the default values of all key-value options above are " // &
      "listed in '" // trim(FILE_PARAM_LIST) // "'."
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
    character(len=*), parameter :: MAJOR_SEPARATOR = repeat("=", TERM_OUT_WIDTH)
    character(len=*), parameter :: MINOR_SEPARATOR = repeat(".", TERM_OUT_WIDTH)

    integer, parameter :: FMT_STR_LEN = 8
    character(len=:), allocatable :: descLenStr     ! Description length
    character(len=:), allocatable :: valueLenStr    ! Value length
    character(len=:), allocatable :: descAbsDistStr ! Dist from left to value
    
    ! Print formatting for parameter descriptions
    character(len=FMT_STR_LEN) :: descFmt
    character(len=FMT_STR_LEN) :: descFmtComplete
    
    ! Print formatting for parameter values
    character(len=FMT_STR_LEN) :: valueIntFmt
    character(len=FMT_STR_LEN) :: valueRealFmt
    character(len=FMT_STR_LEN) :: valueCharFmt
    character(len=FMT_STR_LEN) :: valueCharLineFmt

    character(len=FMT_STR_LEN) :: progNameSpacingStr
    integer :: progNameSpacing
    integer :: k

    descLenStr     = castIntToChar(TERM_OUT_DESC_LEN)
    valueLenStr    = castIntToChar(TERM_OUT_VAL_LEN)
    descAbsDistStr = castIntToChar(TERM_OUT_DESC_LEN + TERM_OUT_VAL_LEN)

    descFmt = "a" // descLenStr
    descFmtComplete = "(" // trim(descFmt) // ")"

    valueIntFmt      = "i" // valueLenStr
    valueRealFmt     = "f" // valueLenStr // ".6"
    valueCharFmt     = "a" // valueLenStr
    valueCharLineFmt = "a" // castIntToChar(TERM_OUT_DESC_LEN + TERM_OUT_VAL_LEN)

    progNameSpacing = aint((TERM_OUT_WIDTH - len_trim(PROG_NAME_ALIAS)) / 2.0)
    if (progNameSpacing > 0) then
      progNameSpacingStr = castIntToChar(progNameSpacing) // "x, "
    else
      progNameSpacingStr = ""
    end if

    ! === Header === !
    print "(a/, " // trim(progNameSpacingStr) // "a/, a)", &
        MAJOR_SEPARATOR , &
        PROG_NAME_ALIAS,  &
        MAJOR_SEPARATOR

    ! ==== Model and program parameters === !
    if (PROG_PRINT_STATE == VERBOSE_PRINT) then
      ! Penna model parameters.
      write(*, "(*(" // descFmt // "," // valueIntFmt // "/))", advance="no") &
          "Genome length",        MODEL_L,              &
          "Mutation threshold",   MODEL_T,              &
          "Birth rate",           MODEL_B,              &
          "Mutation rate",        MODEL_M,              &
          "Min reproduction age", MODEL_R,              &
          "Max reproduction age", MODEL_R_MAX,          &
          "Starting pop size",    MODEL_START_POP_SIZE, &
          "Carrying capacity",    MODEL_K,              &
          "Max time steps",       MODEL_TIME_STEPS
      
      ! New features implemented in this Penna model implementation
      print "(a)", MINOR_SEPARATOR
      write(*, descFmtComplete, advance="no") "Init mutation count"
      if (MODEL_MTTN_COUNT == MTTN_COUNT_RANDOM) then
        print "(" // valueCharFmt // ")", "(random)"
      else
        print "(" // valueIntFmt // ")", MODEL_MTTN_COUNT
      end if

      write(*, descFmtComplete, advance="no") "t-dependent param"
      if (trim(MODEL_TIME_DEPENDENT_PARAM) == TMDP_PARAM_NULL) then  
        print "(" // valueCharFmt // ")", "(none)"
      else
        print "(" // valueCharFmt // ")", trim(MODEL_TIME_DEPENDENT_PARAM)
      end if

      print "(" // descFmt // "," // valueIntFmt // ")", &
          "t-dependent param dt", MODEL_TMDP_PARAM_DELTA_T

      ! Program/Data recording parameters.
      print "(a)", MINOR_SEPARATOR
      print "(" // descFmt // "," // valueIntFmt // ")", &
          "Sample size", PROG_SAMPLE_SIZE
      write(*, "(" // descFmt // ", a)", advance="no") &
          "Data record flag", &
          repeat(" ", TERM_OUT_VAL_LEN - len(REC_FLAG_ORDER))
      do k = 1, len(REC_FLAG_ORDER)
        if (scan(PROG_REC_FLAG, REC_FLAG_ORDER(k:k)) > 0) then
          write(*, "(a1)", advance="no") REC_FLAG_ORDER(k:k)
        else
          write(*, "(a1)", advance="no") "-"
        end if
      end do
      print "(/" // descFmt // "," // valueCharFmt //")", &
          "Data format", trim(PROG_OUT_FMT)
      
      write(*, descFmtComplete, advance="no") "Renyi entropy order"
      if (isFinite64(MODEL_ENTROPY_ORDER)) then
        write(*, "(" // valueRealFmt // "/)", advance="no") &
            MODEL_ENTROPY_ORDER
      else
        write(*, "(" // valueRealFmt // "/, a" // descAbsDistStr // "/)", &
              advance="no") 1.0, "(Normalized)"
      end if

      print "(a)", MAJOR_SEPARATOR
    end if
  end subroutine printParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printVersion
  !>  Print the program version and stop the program.
  ! -------------------------------------------------------------------------- !
  subroutine printVersion()
    print "(a, 1x, a)", trim(PROG_NAME), trim(PROG_VERSION)
    stop
  end subroutine printVersion


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: freeParamAlloctbls
  !>  Free all allocatable objects and objects with allocatable attributes in
  !!  the 'Parameter' module.
  ! -------------------------------------------------------------------------- !
  module subroutine freeParamAlloctbls()
    integer :: i

    ! Free Verhulst weight array.
    if (allocated(MODEL_V_WEIGHT)) deallocate(MODEL_V_WEIGHT)

    ! Free all allocatable attributes of all `CmdArgRecord_t` objects.
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

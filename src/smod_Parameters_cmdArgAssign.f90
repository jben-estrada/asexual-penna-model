submodule (Parameters) CmdArgAssignProcs
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: CmdArgAssignProcs
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `Parameters` containing public procedures with their
  !!  private "helper" procedures for interfacing with other modules, program,
  !!  or other procedures.
  ! -------------------------------------------------------------------------- !
  implicit none

  type(CmdArgParser), target :: pennaCmdArgs
    !! Command argument parser for this program.


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
  end type


  ! All commands.
  ! -------------------------------------------------------------------------- !
  type(CmdArgRecord), target :: cmdArgArr(20)
  ! Model parameters.
  type(CmdArgRecord), pointer :: mttnThreshold_kv => cmdArgArr(1)
  type(CmdArgRecord), pointer :: birthRate_kv     => cmdArgArr(2)
  type(CmdArgRecord), pointer :: mttnRate_kv      => cmdArgArr(3)
  type(CmdArgRecord), pointer :: reprAgeMin_kv    => cmdArgArr(4)
  type(CmdArgRecord), pointer :: reprAgeMax_kv    => cmdArgArr(5)
  type(CmdArgRecord), pointer :: carryingCap_kv   => cmdArgArr(6)
  type(CmdArgRecord), pointer :: genomeLen_kv     => cmdArgArr(7)
  type(CmdArgRecord), pointer :: mttnInitCount_kv => cmdArgArr(8)
  type(CmdArgRecord), pointer :: initPopSize_kv   => cmdArgArr(9)
  type(CmdArgRecord), pointer :: timeStepMax_kv   => cmdArgArr(10)

  ! Program parameters.
  type(CmdArgRecord), pointer :: sampleSize_kv    => cmdArgArr(11)
  type(CmdArgRecord), pointer :: recordData_kv    => cmdArgArr(12)
  type(CmdArgRecord), pointer :: rngChoice_kv     => cmdArgArr(13)
  type(CmdArgRecord), pointer :: rngSeed_kv       => cmdArgArr(14)
  type(CmdArgRecord), pointer :: paramFilePath_kv => cmdArgArr(15)
  type(CmdArgRecord), pointer :: outFilePath_kv   => cmdArgArr(16)
  type(CmdArgRecord), pointer :: showParam_f      => cmdArgArr(17)
  type(CmdArgRecord), pointer :: noParam_f        => cmdArgArr(18)
  type(CmdArgRecord), pointer :: showVersion_f    => cmdArgArr(19)
  type(CmdArgRecord), pointer :: recordTime_f     => cmdArgArr(20)
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initCmdArgRecs
  !>  Initialize `CmdArgRecord` objects in the `cmdArgArr` array.
  ! -------------------------------------------------------------------------- !
  subroutine initCmdArgRecs()
    integer :: i

    ! Model parameters.
    mttnThreshold_kv = CmdArgRecord("T", "mttn-lim", KV_S, KV_L, &
      "Mutation threshold.")
    birthRate_kv     = CmdArgRecord("B", "birth-rate", KV_S, KV_L, &
      "Birth rate.")
    mttnRate_kv      = CmdArgRecord("M", "mttn-rate", KV_S, KV_L, &
      "Mutation rate.")
    reprAgeMin_kv    = CmdArgRecord("r", "r-age-min", KV_S, KV_L, &
      "Minimum reproduction age.")
    reprAgeMax_kv    = CmdArgRecord("R", "r-age-max", KV_S, KV_L, &
      "Maximum reproduction age.")
    carryingCap_kv   = CmdArgRecord("K", "pop-cap", KV_S, KV_L, &
      "Carrying capacity.")
    genomeLen_kv     = CmdArgRecord("L", "genome-len", KV_S, KV_L, &
     "Genome length.")
    mttnInitCount_kv = CmdArgRecord("m", "init-mttn", KV_S, KV_L, &
      "Initial mutation count per individual.")
    initPopSize_kv   = CmdArgRecord("p", "pop-size", KV_S, KV_L, &
      "Initial population size.")
    timeStepMax_kv   = CmdArgRecord("t", "time-step", KV_S, KV_L, &
      "Maximum time step.")

    ! Program parameters.
    sampleSize_kv    = CmdArgRecord("s", "sample-size", KV_S, KV_L, &
      "Sample size.")
    recordData_kv    = CmdArgRecord("d", "record-data", KV_S, KV_L, &
      "Record data. See 'notes' below for recordable data set.")
    rngChoice_kv     = CmdArgRecord("g", "rng-choice", KV_S, KV_L, &
      "Choice of pseduo-RNG. See 'notes' below for available RNGs.")
    rngSeed_kv       = CmdArgRecord("S", "rng-seed", KV_S, KV_L, &
      "RNG seed. Must be integer.")
    paramFilePath_kv = CmdArgRecord("f", "param-file", KV_S, KV_L, &
      "Path to the file containing the default parameter listing.")
    outFilePath_kv   = CmdArgRecord("o", "out", KV_S, KV_L, &
      "Path to file data is to be written in.")
    showParam_f      = CmdArgRecord("v", "verbose", FLAG_S, FLAG_L, &
      "Show all parameters.")
    noParam_f        = CmdArgRecord("q", "quiet", FLAG_S, FLAG_L, &
      "Print nothing when running this program.")
    showVersion_f    = CmdArgRecord("V", "version", FLAG_S, FLAG_L, &
      "Show the version of this program.")
    recordTime_f     = CmdArgRecord("i", "record-time", FLAG_S, FLAG_L, &
      "Record timing statistics.")

    ! Assign pointers.
    mttnThreshold_kv % intValue_ptr => MODEL_T
    birthRate_kv     % intValue_ptr => MODEL_B
    mttnRate_kv      % intValue_ptr => MODEL_M
    reprAgeMin_kv    % intValue_ptr => MODEL_R
    reprAgeMax_kv    % intValue_ptr => MODEL_R_MAX
    carryingCap_kv   % intValue_ptr => MODEL_K
    genomeLen_kv     % intValue_ptr => MODEL_L
    mttnInitCount_kv % intValue_ptr => MODEL_MTTN_COUNT
    initPopSize_kv   % intValue_ptr => MODEL_START_POP_SIZE
    timeStepMax_kv   % intValue_ptr => MODEL_TIME_STEPS

    sampleSize_kv % intValue_ptr => PROG_SAMPLE_SIZE
    rngChoice_kv  % intValue_ptr => PROG_RNG
    rngSeed_kv    % intValue_ptr => PROG_RNG_SEED

    recordData_kv    % charValue_ptr => PROG_REC_FLAG
    paramFilePath_kv % charValue_ptr => FILE_PARAM_LIST
    outFilePath_kv   % charValue_ptr => PROG_OUT_FILE_NAME

    ! Set all initialized commands in this routine.
    do i = 1, size(cmdArgArr)
      call pennaCmdArgs % setCmd( &
        cmdArgArr(i) % cmdName,     &
        cmdArgArr(i) % cmdType,     &
        cmdArgArr(i) % usageTxt,    &
        cmdArgArr(i) % cmdAlias,    &
        cmdArgArr(i) % cmdAliasType &
        )
    end do
  end subroutine initCmdArgRecs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setParams
  !>  Assign parameters from external file and command arguments.
  ! -------------------------------------------------------------------------- !
  subroutine setParams()
    ! Initialize the command argument parser.
    call pennaCmdArgs % init()
    call initCmdArgRecs()

    call pennaCmdArgs % readCmdArgs()

    ! Prefix to file name the relative path to the program executable.
    ! The default parameter file is in the same directory as the executable.
    FILE_PARAM_LIST = prefixRelFilePath(FILE_PARAM_LIST)

    ! Check if the help message is to be printed. Printing the help message has
    ! precendence over initialization of program to help first-time users to
    ! troubleshoot problems.
    if (pennaCmdArgs % isFlagToggled("help")) call printHelpAndNotesMsgs()

    ! Get the flle path for the parameter listing file.
    if (pennaCmdArgs % hasValue(paramFilePath_kv % cmdName)) then
      paramFilePath_kv % charValue_ptr = &
        pennaCmdArgs % getCmdValue(paramFilePath_kv % cmdName)
    end if

    ! Read the model parameters from the config files.
    call readDefaultParamVal()

    ! Assign the parameters obtained from command arguments.
    call assignUserProvidedParams()
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

        if (.not.(progPath(1:1) == "/" .or. progPath(1:2) == "./")) &
            relPath = "./" // relPath
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
  subroutine assignUserProvidedParams()
    type(CmdArgRecord) :: currCmdArg
    integer :: i, castStat

    do i = 1, size(cmdArgArr)
      currCmdArg = cmdArgArr(i)

      ! Handle key-value commands.
      if (currCmdArg % cmdType == KV_L .or. currCmdArg % cmdType == KV_S) then
        if (associated(currCmdArg % intValue_ptr)) then
          ! Check if the current key-value command has a value.
          if (pennaCmdArgs % hasValue(currCmdArg % cmdName)) then
            ! Get value and cast it to integer.
            currCmdArg % intValue_ptr = castCharToInt( &
              pennaCmdArgs % getCmdValue(currCmdArg % cmdName), castStat &
            )
          end if

        else if (associated(currCmdArg % charValue_ptr)) then
          ! Check if the current key-value command has a value.
          if (pennaCmdArgs % hasValue(currCmdArg % cmdName)) then
            currCmdArg % charValue_ptr = &
              pennaCmdArgs % getCmdValue(currCmdArg % cmdName)
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
        ! NOTE: Apparently using `select case` on this one does not work.
        if (currCmdArg % cmdName == showParam_f % cmdName) then
          if (pennaCmdArgs % isFlagToggled(showParam_f % cmdName)) then
            PROG_PRINT_STATE = VERBOSE_PRINT
          end if

        else if (currCmdArg % cmdName == noParam_f % cmdName) then
          if (pennaCmdArgs % isFlagToggled(noParam_f % cmdName)) then
            PROG_PRINT_STATE = SILENT_PRINT
          end if

        else if (currCmdArg % cmdName == showVersion_f % cmdName) then
          if (pennaCmdArgs % isFlagToggled(showVersion_f % cmdName)) then
            PROG_PRINT_STATE = VERSION_PRINT
          end if

        else if (currCmdArg % cmdName == recordTime_f % cmdName) then
          PROG_RECORD_TIME = &
            pennaCmdArgs % isFlagToggled(recordTime_f % cmdName)

        else
          call raiseError( &
              "Unknown flag command '" //  currCmdArg % cmdName // "'." &
            )
        end  if
      end if
    end do

    call checkValidParams()
  end subroutine assignUserProvidedParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkValidParams
  !>  Check the assigned model parameters for invalid values.
  ! -------------------------------------------------------------------------- !
  subroutine checkValidParams()
    logical :: printNoParam, printAllParam

    printNoParam = pennaCmdArgs % isFlagToggled(noParam_f % cmdName)
    printAllParam = pennaCmdArgs % isFlagToggled(showParam_f % cmdName)

    ! Check invalid combination of flags.
    if (printNoParam .and. printAllParam) then

      call raiseError( &
        "Flag options for verbose print and silent print cannot be chosen " // &
        "together." &
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
  subroutine printHelpAndNotesMsgs()
    character(len=:), allocatable :: progName

    ! Initialize local variables.
    allocate(character(len=0) :: progName)
    progName = getProgName()

    call pennaCmdArgs % printHelp(progName, PROG_DESC)

    ! Print the additional notes.
    print "(/a)", "Notes:"
    print "(' ', *(a))", &
      "Valid character values for -", recordData_kv % cmdName, &
      " or --", recordData_kv % cmdAlias
    write(*, "(6(2(' '), a/))", advance="no") &
      "x - Record nothing.", &
      "p - Population size per time step", &
      "a - Age demographics in the last 300 time step.", &
      "d - Death count per time step.", &
      "s - Shannon diversity index of genomes per time step.", &
      "b - Bad gene distribution per time step."
    print "(' ', *(a))", &
      "Valid integer values for -", rngChoice_kv % cmdName, &
      " or --", rngChoice_kv % cmdAlias
    write(*, "(2(2(' '), a/))", advance="no") &
      "0 - xoshiro256** pseudo-RNG (Fortran intrinsic RNG)", &
      "1 - Mersenne twister (MT19937) pseudo-RNG"

    print "(/' ', *(a))", &
      "Default values of all key-value options above are, " // &
      "by default, listed in '", trim(FILE_PARAM_LIST), &
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
    use ANSIEscCodes, only: formatChar, escCodeBold

    ! Pretty print separator.
    integer :: k
    character, parameter :: PRINT_SEPARATOR(*) = [("=", k = 1, 29)]

    ! ***Header
    print "(*(a))", PRINT_SEPARATOR 
    print "(a)", formatChar("Asexual Penna model", escCodeBold)
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

      write(*, "(*(a20, 8(' '), a1/))", advance="no") &
          "Record flag", PROG_REC_FLAG

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

    ! Free the attributes of command argument parser.
    call pennaCmdArgs % free()

    ! Free all allocatable attributes of all `CmdArgRecord` objects.
    do i = lbound(cmdArgArr, 1), ubound(cmdArgArr, 1)
      ! Free allocatable character attributes.
      if (allocated(cmdArgArr(i) % cmdName)) deallocate(cmdArgArr(i) % cmdName)
      if (allocated(cmdArgArr(i) % cmdAlias)) deallocate(cmdArgArr(i) %cmdAlias)
      if (allocated(cmdArgArr(i) % usageTxt)) deallocate(cmdArgArr(i) %usageTxt)

      ! Nullify pointer attributes.
      cmdArgArr(i) % charValue_ptr => null()
      cmdArgArr(i) % intValue_ptr => null()
    end do
  end subroutine freeParamAlloctbls
end submodule CmdArgAssignProcs

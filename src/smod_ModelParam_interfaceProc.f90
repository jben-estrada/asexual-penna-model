submodule (ModelParam) InterfaceProcedures
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: interfaceProcedures
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `ModelParam` containing public procedures with their
  !!  private "helper" procedures for interfacing with other modules, program,
  !!  or other procedures.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignConfigFilePaths
  !>  Get paths for configuration files.
  ! -------------------------------------------------------------------------- !
  subroutine assignConfigFilePaths()
    character(len=MAX_LEN) :: filePathExec
    logical :: exist

    ! Get the paths of config files relative to the executable.
    call get_command_argument(0, filePathExec)
    call getRelFilePath(filePathExec, FILE_NAME_MODEL)
    call getRelFilePath(filePathExec, FILE_NAME_VWEIGHT)

    ! Assign the default config file paths.
    call assignOptionalPosTypeVal(configDirPosArg, FILE_NAME_MODEL)
    call assignOptionalPosTypeVal(vWeightDirPosArg, FILE_NAME_VWEIGHT)
    
    ! Get the positional command-line arguments.
    call parseCmdArgs(.false., .false., .true.)

    ! Get the config file paths from command-line arguments.
    FILE_NAME_MODEL = configDirPosArg % getValue()
    FILE_NAME_VWEIGHT = vWeightDirPosArg % getValue()

    ! Inquire the existence of the config file for model paramters.
    inquire(file=FILE_NAME_MODEL, exist=exist)
    if (.not. exist) then
      print "(3a)", "***ERROR. The file '", trim(FILE_NAME_MODEL), &
          "' cannot be opened or does not exist."
      error stop
    end if

    ! Inquire the existence of the config file for Verhulst weights.
    inquire(file=FILE_NAME_VWEIGHT, exist=exist)
    if (.not. exist) then
      print "(3a)", "***ERROR. The file '", trim(FILE_NAME_VWEIGHT), &
          "' cannot be opened or does not exist." 
      error stop
    end if
  end subroutine assignConfigFilePaths


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: getRelFilePath
  !>  Get paths of `replacement` relative to `source`. 
  ! -------------------------------------------------------------------------- !
  subroutine getRelFilePath(source, replacement)
    character(len=*),       intent(in)    :: source
    character(len=*),       intent(inout) :: replacement

    integer :: i

    do i = len(source), 1, -1
      ! Find the last file path delimiter, i.e. the delimeter before the file
      ! name of the executable.
      if (source(i:i) == "/") then
        ! Concatenate the relative path to the executable and the replacement
        ! file name.
        replacement = source(1:i) // trim(replacement)
        return
      end if
    end do

    ! Raise an error if no file path delimiter was found.
    ! NOTE: The delimiter used is `/`. This may differ in other shells.
    print "(3a)", "***ERROR. '", trim(source), "' is not a valid path."
    error stop
  end subroutine getRelFilePath


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignModelParams
  !>  Assign model parameters from command-line arguments. The value of a 
  !!  model parameter should not change if the correspoding command-line
  !!  option is not passed.
  ! -------------------------------------------------------------------------- !
  subroutine assignModelParams()
    use CastProcedures

    ! Read the model parameters from the config files.
    call readScalarModelParamCfg()
    call readVerhulstWeightsCfg()

    ! Assign optional values to key-value options.
    call assignOptionalKVVal(maxTimeStepArg, castIntToChar(MODEL_TIME_STEPS))
    call assignOptionalKVVal(sampleSizeArg, castIntToChar(PROG_SAMPLE_SIZE))
    call assignOptionalKVVal(startPopSizeArg, &
        castIntToChar(MODEL_START_POP_SIZE))
    call assignOptionalKVVal(recordFlagArg, PROG_REC_FLAG)
    call assignOptionalKVVal(rngChoiceArg, castIntToChar(PROG_RNG))
    call assignOptionalKVVal(rngSeedArg, castIntToChar(PROG_RNG_SEED))

    ! Get key-value command-line arguments w/c so happens to only contain
    ! model parameters.
    call parseCmdArgs(.false., .true., .false.)
    
    ! Assign model parameters from the command-line arguments.
    MODEL_TIME_STEPS = castCharToInt(maxTimeStepArg % getValue())
    PROG_SAMPLE_SIZE = castCharToInt(sampleSizeArg % getValue())
    MODEL_START_POP_SIZE = castCharToInt(startPopSizeArg % getValue())
    PROG_REC_FLAG = adjustl(recordFlagArg % getValue())
    PROG_RNG = castCharToInt(rngChoiceArg % getValue())
    PROG_RNG_SEED = castCharToInt(rngSeedArg % getValue())

    ! Get the flag command-line arguments.
    call parseCmdArgs(.true., .false., .false.)

    ! Get values of print flags.
    if (verbosePrintFlag % getFlagState()) then
      PROG_PRINT_STATE = VERBOSE_PRINT
    else if (silentPrintFlag % getFlagState()) then
      PROG_PRINT_STATE = SILENT_PRINT
    else if (versionPrintFlag % getFlagState()) then
      PROG_PRINT_STATE = VERSION_PRINT 
    end if

    ! Record time flag.
    PROG_RECORD_TIME = recordTimeFlag % getFlagState()

    ! Check for errors and invalid passed arguments.
    call checkValidModelParams()
  end subroutine assignModelParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkValidModelParams
  !>  Check the assigned model parameters for invalid values.
  ! -------------------------------------------------------------------------- !
  subroutine checkValidModelParams()
    ! Check invalid combination of flags.
    if (silentPrintFlag % getFlagState() .and. &
        verbosePrintFlag % getFlagState()) then
      print "(*(a))", "***ERROR. '", trim(verbosePrintFlag % getCommand()), &
          "' or '", trim(verbosePrintFlag % getAltCommand()), &
          "' cannot be passed with '", trim(silentPrintFlag % getCommand()), &
          "' or '", trim(silentPrintFlag % getAltCommand()), "'."
      error stop
    end if
  end subroutine checkValidModelParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printModelParams
  !>  Pretty print the model parameters. Can print verbosely or print nothing
  !!  if need be.
  ! -------------------------------------------------------------------------- !
  subroutine printProgDetails()
    select case (PROG_PRINT_STATE)
      case (NORMAL_PRINT, VERBOSE_PRINT)
        call printModelParams()

      case (VERSION_PRINT)
        call printVersion()
      
      case (SILENT_PRINT)
        ! Do nothing

      case default
        print "(a)", "***ERROR. Invalid 'PROG_PRINT_STATE' value."
        error stop
      end select
  end subroutine printProgDetails


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printModelParams
  !>  Pretty print the model parameters. Can print verbosely or print nothing
  !!  if need be.
  ! -------------------------------------------------------------------------- !
  subroutine printModelParams()
    use WriterOptions, only: nullFlag

    ! Pretty print separator.
    integer :: k
    character, parameter :: PRINT_SEPARATOR(*) = [("=", k = 1, 29)]

    ! ***Header
    print "(*(a))", PRINT_SEPARATOR 
    print "(a)", "Asexual Penna model"
    print "(*(a))", PRINT_SEPARATOR

    ! ***Body (Extended model parameters)
    if (PROG_PRINT_STATE == VERBOSE_PRINT) then
      write(*, "(*(a20, i9/))", advance="no") &
      "Genome length",        MODEL_L, &
      "Mutation threshold",   MODEL_T, &
      "Birth rate",           MODEL_B, &
      "Mutation rate",        MODEL_M, &
      "Min reproduciton age", MODEL_R, &
      "Max reproduction age", MODEL_R_MAX, &
      "Carrying capacity",    MODEL_K
    end if

    ! ***Body
    write(*, "(*(a20, i9/))", advance="no") &
      "Number of time steps", MODEL_TIME_STEPS, &
      "Sample size",          PROG_SAMPLE_SIZE,  &
      "Starting pop size",    MODEL_START_POP_SIZE
    print "(a20, L9)", "Record result", PROG_REC_FLAG /= nullFlag

    ! ***End
    print "(*(a))", PRINT_SEPARATOR
  end subroutine printModelParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: printVersion
  !>  Print the program version and stop the program.
  ! -------------------------------------------------------------------------- !
  subroutine printVersion()
    print "(a, ' ', a)", trim(PROG_NAME), trim(PROG_VERSION)
    stop
  end subroutine printVersion


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocVerhulstWeights
  !>  Deallocate `MODEL_VERHULST_W` array.
  ! -------------------------------------------------------------------------- !
  subroutine deallocVerhulstWeights()
    if (allocated(MODEL_VERHULST_W)) deallocate(MODEL_VERHULST_W)
  end subroutine deallocVerhulstWeights
end submodule
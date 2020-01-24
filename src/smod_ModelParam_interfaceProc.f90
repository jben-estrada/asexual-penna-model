submodule (ModelParam) InterfaceProcedures
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignConfigFilePaths
  !>  Get paths for configuration files.
  ! -------------------------------------------------------------------------- !
  subroutine assignConfigFilePaths()
    logical :: exist

    ! Assign the default config file paths.
    call assignOptionalPosTypeVal(configDirPosArg, MODEL_FILE_NAME)
    call assignOptionalPosTypeVal(vWeightDirPosArg, VWEIGHT_FILE_NAME)
    
    ! Get the positional command-line arguments.
    call parseCmdArgs(.false., .false., .true.)

    ! Get the config file paths from command-line arguments.
    MODEL_FILE_NAME = configDirPosArg % getValue()
    VWEIGHT_FILE_NAME = vWeightDirPosArg % getValue()

    ! Inquire the existence of the config file for model paramters.
    inquire(file=MODEL_FILE_NAME, exist=exist)
    if (.not. exist) then
      print "(3a)", "***ERROR. The file '", trim(MODEL_FILE_NAME), &
          "' cannot be opened or does not exist."
      print "(a)", "   Try 'penna.out -h' for more info " // &
          "if this does not intend to be a file or directory."
      stop
    end if

    ! Inquire the existence of the config file for Verhulst weights.
    inquire(file=VWEIGHT_FILE_NAME, exist=exist)
    if (.not. exist) then
      print "(3a)", "***ERROR. The file '", trim(VWEIGHT_FILE_NAME), &
          "' cannot be opened or does not exist." 
      print "(a)", "   Try 'penna.out -h' for more info " // &
          "if this does not intend to be a file or directory."
      stop
    end if
  end subroutine assignConfigFilePaths


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignModelParams
  !>  Assign model parameters from command-line arguments. The value of a 
  !!  model parameter should not change if the correspoding command-line
  !!  option is not passed.
  ! -------------------------------------------------------------------------- !
  subroutine assignModelParams()
    ! Read the model parameters from the config files.
    call readScalarModelParamCfg()
    call readVerhulstWeightsCfg()

    ! Assign optional values to key-value options.
    call assignOptionalKVVal(maxTimeStepArg, MODEL_TIME_STEPS)
    call assignOptionalKVVal(sampleSizeArg, MODEL_SAMPLE_SIZE)
    call assignOptionalKVVal(startPopSizeArg, MODEL_N0)
    call assignOptionalKVVal(recordFlagArg, MODEL_REC_FLAG)
    call assignOptionalKVVal(rngChoiceArg, MODEL_RNG)
    call assignOptionalKVVal(rngSeedArg, MODEL_RNG_SEED)

    ! Get key-value command-line arguments w/c so happens to only contain
    ! model parameters.
    call parseCmdArgs(.false., .true., .false.)

    ! Assign model parameters from the command-line arguments.
    MODEL_TIME_STEPS = maxTimeStepArg % getValue()
    MODEL_SAMPLE_SIZE = sampleSizeArg % getValue()
    MODEL_N0 = startPopSizeArg % getValue()
    MODEL_REC_FLAG = recordFlagArg % getValue()
    MODEL_RNG = rngChoiceArg % getValue()
    MODEL_RNG_SEED = rngSeedArg % getValue()

    ! Get the flag command-line arguments.
    call parseCmdArgs(.true., .false., .false.)

    ! Print flags.
    if (verbosePrintFlag % getFlagState()) PRINT_STATE = VERBOSE_PRINT
    if (silentPrintFlag % getFlagState()) PRINT_STATE = SILENT_PRINT

    ! Record time flag.
    RECORD_TIME = recordTimeFlag % getFlagState()

    ! Check for errors and invalid passed arguments.
    call checkValidModelParams()
  end subroutine assignModelParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkValidModelParams
  !>  Check the assigned model parameters for invalid values.
  ! -------------------------------------------------------------------------- !
  subroutine checkValidModelParams()
    character(len=*), parameter :: MODIFYABLE_PARAM_CHAR(*) = &
      ["maximum time step       ", &
      "sample size             ", &
      "starting population size"]
    integer, parameter :: MODIFYABLE_PARAM_IDX(size(MODIFYABLE_PARAM_CHAR)) = &
      [9, 10, 8]
    integer :: i

    ! Check invalid combination of flags.
    if (silentPrintFlag % getFlagState() .and. &
        verbosePrintFlag % getFlagState()) then
      print "(*(a))", "***ERROR. '", trim(verbosePrintFlag % getCommand()), &
          "' or '", trim(verbosePrintFlag % getAltCommand()), &
          "' cannot be passed with '", trim(silentPrintFlag % getCommand()), &
          "' or '", trim(silentPrintFlag % getAltCommand()), "'."
      stop
    end if

    ! Check for non-positive model parameters; all numerical model parameters
    ! must be positive as of now.
    do i = 1, size(MODIFYABLE_PARAM_CHAR)
      if (modelParams(MODIFYABLE_PARAM_IDX(i)) <= 0) then
        print "(3a)", "***ERROR. The '", trim(MODIFYABLE_PARAM_CHAR(i)), &
            "' must be a positive integer."
        stop
      end if
    end do
  end subroutine checkValidModelParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: prettyPrintModelParams
  !>  Pretty print the model parameters. Can print verbosely or print nothing
  !!  if need be.
  ! -------------------------------------------------------------------------- !
  subroutine prettyPrintModelParams()
    use WriterOptions, only: nullFlag

    ! Pretty print separator.
    integer :: k
    character, parameter :: PRINT_SEPARATOR(*) = [("=", k = 1, 29)]

    ! Skip the argument printing.
    if (PRINT_STATE == SILENT_PRINT) return

    ! ***Header
    print "(*(a))", PRINT_SEPARATOR 
    print "(a)", "Asexual Penna model"
    print "(*(a))", PRINT_SEPARATOR

    ! ***Body (Extended model parameters)
    if (PRINT_STATE == VERBOSE_PRINT) then
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
      "Sample size",          MODEL_SAMPLE_SIZE,  &
      "Starting pop size",    MODEL_N0
    print "(a20, L9)", "Record result", MODEL_REC_FLAG /= nullFlag

    ! ***End
    print "(*(a))", PRINT_SEPARATOR
  end subroutine prettyPrintModelParams


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: deallocVerhulstWeights
  !>  Deallocate `MODEL_VERHULST_W` array.
  ! -------------------------------------------------------------------------- !
  subroutine deallocVerhulstWeights()
    if (allocated(MODEL_VERHULST_W)) deallocate(MODEL_VERHULST_W)
  end subroutine deallocVerhulstWeights
end submodule
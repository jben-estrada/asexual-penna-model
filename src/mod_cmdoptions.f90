module CmdOptions
  ! -------------------------------------------------------------------------- !
  ! MODULE: CmdOptions
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Module containing declaration of `BaseCmdOption` instances and wrapper
  !!  procedures to interface with the main program, and modules and procedures.
  ! -------------------------------------------------------------------------- !
  use CmdOptionType
  implicit none
  private

  ! Flag options.
  type(FlagCmdOption), target :: cmdFlags(5)
  type(FlagCmdOption), pointer, protected, public :: versionPrintFlag => &
    cmdFlags(1)
  type(FlagCmdOption), pointer, protected, public :: verbosePrintFlag => &
    cmdFlags(2)
  type(FlagCmdOption), pointer, protected, public :: showHelpMsgFlag => &
    cmdFlags(3)
  type(FlagCmdOption), pointer, protected, public :: recordTimeFlag => &
    cmdFlags(4)
  type(FlagCmdOption), pointer, protected, public :: silentPrintFlag => &
    cmdFlags(5)

  ! Key-value arguments: Model parameters.
  type(KeyValCmdOption), target :: cmdKeyVal(6)
  type(KeyValCmdOption), pointer, protected, public :: maxTimeStepArg => &
    cmdKeyVal(1)
  type(KeyValCmdOption), pointer, protected, public :: sampleSizeArg => &
    cmdKeyVal(2)
  type(KeyValCmdOption), pointer, protected, public :: startPopSizeArg => &
    cmdKeyVal(3)

  ! Key-value arguments: Other program parameters.
  type(KeyValCmdOption), pointer, protected, public :: recordFlagArg => &
    cmdKeyVal(4)
  type(KeyValCmdOption), pointer, protected, public :: rngChoiceArg => &
    cmdKeyVal(5)
  type(KeyValCmdOption), pointer, protected, public :: rngSeedArg => &
    cmdKeyVal(6)

  ! Positional arguments.
  type(PositionalCmdOption), target :: cmdPosArgs(2)
  type(PositionalCmdOption), pointer, protected, public :: configDirPosArg => &
    cmdPosArgs(1)
  type(PositionalCmdOption), pointer, protected, public :: vWeightDirPosArg => &
    cmdPosArgs(2)

  public :: initializeCmdOptions
  public :: parseCmdArgs
  public :: showHelpMsgAndNotes

  ! Routines for assigning optional values.
  public :: assignOptionalKVVal
  public :: assignOptionalPosTypeVal
  public :: assignInitialFlagState
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeCmdOptions
  !>  Initialize command-line option types.
  ! -------------------------------------------------------------------------- !
  subroutine initializeCmdOptions()
    ! Assign command char of flag options.
    call initializeCmdOption(versionPrintFlag, "version", "V")
    call initializeCmdOption(verbosePrintFlag, "verbose", "v")
    call initializeCmdOption(showHelpMsgFlag, "help", "h")
    call initializeCmdOption(recordTimeFlag, "record-time", "t")
    call initializeCmdOption(silentPrintFlag, "quiet", "q")
  
    ! Assign command char of key-value options.
    call initializeCmdOption(maxTimeStepArg, "time-step", "T")
    call initializeCmdOption(sampleSizeArg, "sample-size", "s")
    call initializeCmdOption(startPopSizeArg, "pop-size", "p")
    call initializeCmdOption(recordFlagArg, "record", "r")
    call initializeCmdOption(rngChoiceArg, "rng", "R")
    call initializeCmdOption(rngSeedArg, "seed", "S")

    ! Assign command char and position of position arguments.
    call initializeCmdOption(configDirPosArg, "model-param-path")
    call initializeCmdOption(vWeightDirPosArg, "vweight-path")
    call setPosTypePosition(configDirPosArg, 1)
    call setPosTypePosition(vWeightDirPosArg, 2)

    call setCmdOptionUsageMsgs()
  end subroutine initializeCmdOptions


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setCmdOptionUsageMsgs
  !>  Assign usage messages to module-defined command-line option objects.
  ! -------------------------------------------------------------------------- !
  subroutine setCmdOptionUsageMsgs()
    ! Set usage messages of flag options.
    call setUsageMsg(versionPrintFlag, "Show the version information and exit.")
    call setUsageMsg(verbosePrintFlag, "Show all the model parameters.")
    call setUsageMsg(showHelpMsgFlag, "Show this message and exit.")
    call setUsageMsg(recordTimeFlag, "Record the average elapsed time " // &
        "of the simulation.")
    call setUsageMsg(silentPrintFlag, "Quietly run the simulation.")

    ! Set usage messages of key-value options.
    call setUsageMsg(maxTimeStepArg, "Maximum time step.")
    call setUsageMsg(sampleSizeArg, "Sample size.")
    call setUsageMsg(startPopSizeArg, "Starting population size.")
    call setUsageMsg(recordFlagArg, "Record data as specified by the " // &
        "given character value.")
    call setUsageMsg(rngChoiceArg, "Choose an RNG to be used as specified " // &
        "by integer flag.")
    call setUsageMsg(rngSeedArg, "Set the seed for the RNG.")

    ! Set the message for the value in key-value options.
    call setValueMsg(maxTimeStepArg, "<int>")
    call setValueMsg(sampleSizeArg, "<int>")
    call setValueMsg(startPopSizeArg, "<int>")
    call setValueMsg(recordFlagArg, "<str>")
    call setValueMsg(rngChoiceArg, "<int>")
    call setValueMsg(rngSeedArg, "<int>")

    ! Set usage messages of positional arguments.
    call setUsageMsg(configDirPosArg, "Path of text file of " // &
        "model parameters.")
    call setUsageMsg(vWeightDirPosArg, "Path of text file of " // &
        "Verhulst weights.")
  end subroutine setCmdOptionUsageMsgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseNonPosArgs
  !>  Parse key-value arguments and flags.
  ! -------------------------------------------------------------------------- !
  subroutine parseCmdArgs(readFlag, readKeyVal, readPosArg)    
    logical, intent(in)    :: readFlag
    logical, intent(in)    :: readKeyVal
    logical, intent(in)    :: readPosArg

    call parsePassedCmdArgs(cmdFlags, cmdKeyVal, cmdPosArgs, &
        readFlag, readKeyVal, readPosArg)
  end subroutine parseCmdArgs


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: showHelpMsgAndNotes
  !>  Show the help message together with notes with regards to char/int flags.
  ! -------------------------------------------------------------------------- !
  subroutine showHelpMsgAndNotes()
    use WriterOptions

    if (showHelpMsgFlag % getFlagState()) then
      call showHelpMsg(cmdFlags, cmdKeyVal, cmdPosArgs)

      ! Print notes:
      print "(/a)", "notes:"
      write(*, "(4(' '), a/, 6(8(' '), a/))", advance="no") &
          "Valid char values for '--record' or '-r': ", &
          nullFlag     // " - record nothing", &
          popFlag      // " - population count per time step", &
          ageDstrbFlag // " - age distribution", &
          deathFlag    // " - death count", &
          divIdxFlag   // " - Shannon diversity index per time step", &
          badGeneFlag  // " - Bad gene distribution per time step"
      write(*, "(4(' '), a/, 2(8(' '), a/))", advance="no") &
          "Valid integer flags for '--rng' or '-R': ", &
          "0 - xoshiro256** pseudo-RNG (the intrinsic RNG)", &
          "1 - Mersenne twister (MT19937) pseudo-RNG"
      stop
    end if
  end subroutine showHelpMsgAndNotes
end module CmdOptions

module CmdOptions
  use CmdOptionType
  implicit none
  private

  ! Flag options.
  type(FlagCmdOption), target :: cmdFlags(4)
  type(FlagCmdOption), pointer, protected, public :: verbosePrintFlag => &
    cmdFlags(1)
  type(FlagCmdOption), pointer, protected, public :: showHelpMsgFlag => &
    cmdFlags(2)
  type(FlagCmdOption), pointer, protected, public :: recordTimeFlag => &
    cmdFlags(3)
  type(FlagCmdOption), pointer, protected, public :: silentPrintFlag => &
    cmdFlags(4)

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
  public :: parseCommandArguments
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeCmdOptions
  !>  Initialize command-line option types.
  ! -------------------------------------------------------------------------- !
  subroutine initializeCmdOptions()
    use ModelParam
    implicit none

    ! Assign command char of flag options.
    call initializeCmdOption(verbosePrintFlag, "-v", "--verbose")
    call initializeCmdOption(showHelpMsgFlag, "-h", "--help")
    call initializeCmdOption(recordTimeFlag, "-r", "--record-time")
    call initializeCmdOption(silentPrintFlag, "-s", "--silent")
  
    ! Assign command char of key-value options.
    call initializeCmdOption(maxTimeStepArg, "--max-time-step")
    call initializeCmdOption(sampleSizeArg, "--sample-size")
    call initializeCmdOption(startPopSizeArg, "--pop-size")
    call initializeCmdOption(recordFlagArg, "--rec-flag")
    call initializeCmdOption(rngChoiceArg, "--rng")
    call initializeCmdOption(rngSeedArg, "--seed")

    ! Assign command char and position of position arguments.
    call initializeCmdOption(configDirPosArg, "config-directory")
    call initializeCmdOption(vWeightDirPosArg, "vweight-directory")
    call setPosTypePosition(configDirPosArg, 1)
    call setPosTypePosition(vWeightDirPosArg, 2)

    ! Set usage messages of flag options.
    call setUsageMsg(verbosePrintFlag, "Show all the model parameters.")
    call setUsageMsg(showHelpMsgFlag, "Show this message and exit.")
    call setUsageMsg(recordTimeFlag, "Record the average elapsed time " // &
        "of the simulation.")
    call setUsageMsg(silentPrintFlag, "Do not show the model parameters.")

    ! Set usage messages of key-value options.
    call setUsageMsg(maxTimeStepArg, "Maximum time step.")
    call setUsageMsg(sampleSizeArg, "Sample size.")
    call setUsageMsg(startPopSizeArg, "Starting population size.")
    call setUsageMsg(recordFlagArg, "Record data specified by the " // &
        "given integer flag.")
    call setUsageMsg(rngChoiceArg, "Choose a random number " // &
        "generator (RNG) to be used as specified by the given" // &
        " integer flag.")
    call setUsageMsg(rngSeedArg, "Set the seed for the RNG.")

    ! Set usage messages of positional arguments.
    call setUsageMsg(configDirPosArg, "Directory of .cfg file of " // &
        "model parameters.")
    call setUsageMsg(vWeightDirPosArg, "Directory of .cfg file of " // &
        "Verhulst weights.")
  end subroutine initializeCmdOptions


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: parseCommandArguments
  !>  Parse command-line arguments with the defined CLI options in 'CmdOptions'
  !!  This is essentially a wrapper to the 'parseCmdArgs' subroutine.
  ! -------------------------------------------------------------------------- !
  subroutine parseCommandArguments()
    implicit none
    call parseCmdArgs(cmdFlags, cmdKeyVal, cmdPosArgs)
  end subroutine parseCommandArguments
end module CmdOptions

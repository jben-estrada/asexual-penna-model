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
    call verbosePrintFlag % setCommand("-v", "--verbose")
    call showHelpMsgFlag % setCommand("-h", "--help")
    call recordTimeFlag % setCommand("-r", "--record-time")
    call silentPrintFlag % setCommand("-s", "--silent")
  
    ! Assign command char of key-value options.
    call maxTimeStepArg % setCommand("-max-time-step")
    call sampleSizeArg % setCommand("-sample-size")
    call startPopSizeArg % setCommand("-pop-size")
    call recordFlagArg % setCommand("-rec-flag")
    call rngChoiceArg % setCommand("-rng")
    call rngSeedArg % setCommand("-seed")

    ! Assign commansd char and position of position arguments.
    call configDirPosArg % setCommand("config-directory")
    call configDirPosArg % setPosition(1)
    call vWeightDirPosArg % setCommand("vweight-directory")
    call vWeightDirPosArg % setPosition(2)

    ! Set usage messages of flag options.
    call verbosePrintFlag % setUsageMsg("Show all the model parameters.")
    call showHelpMsgFlag % setUsageMsg("Show this message and exit.")
    call recordTimeFlag % setUsageMsg("Record the average elapsed time " // &
      "of the simulation.")
    call silentPrintFlag % setUsageMsg("Do not show the model parameters.")

    ! Set usage messages of key-value options.
    call maxTimeStepArg % setUsageMsg("Maximum time step.")
    call sampleSizeArg % setUsageMsg("Sample size.")
    call startPopSizeArg % setUsageMsg("Starting population size.")
    call recordFlagArg % setUsageMsg("Record data specified by the " // &
        "given integer flag.")
    call rngChoiceArg % setUsageMsg("Choose a random number " // &
        "generator (RNG) to be used as specified by the given" // &
        " integer flag.")
    call rngSeedArg % setUsageMsg("Set the seed for the RNG.")

    ! Set usage messages of positional arguments.
    call configDirPosArg % setUsageMsg("Directory of .cfg file of " // &
        "model parameters.")
    call vWeightDirPosArg % setUsageMsg("Directory of .cfg file of " // &
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

submodule (Penna) RunWriterHandling
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: RunWriterHandling
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `Penna` containing the procedures for handling the `Writer`
  !!  object for writing data obtained from Penna model simulations. 
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: initializeRunWriter
  !>  Initialize a `Writer` object based on the integer `recordFlag`
  !!  passed. The flags are defined in the `SaveFormat` module.
  ! -------------------------------------------------------------------------- !
  subroutine initializeRunWriter(runWriter, recordFlag)
    type(Writer), intent(inout) :: runWriter
      !! The `Writer` object to be initialized.
    character,    intent(in)    :: recordFlag
      !! Record flag. Values can be found in `WriterOptions`.

    if (recordFlag == nullFlag) return

    ! Construct the `Writer` type.
    call constructAvailableWriter(runWriter, &
        [popFlag, ageDstrbFlag, deathFlag, divIdxFlag, badGeneFlag])

    call runWriter % initialize(recordFlag)
    select case (recordFlag)
      case (popFlag)
        call runWriter % writeHeader(popFlag, ["population size"])

      case (ageDstrbFlag)
        call runWriter % writeHeader(ageDstrbFlag, ["age =>"])

      case (deathFlag)
        call runWriter % writeHeader(deathFlag, &
            ["death by old age        ", &
            "death by mutation       ", &
            "death by Verhulst factor"])

      case (divIdxFlag)
        call runWriter % writeHeader(divIdxFlag, &
            ["Diversity index per time step"])
      
      case (badGeneFlag)
        call runWriter % writeHeader(badGeneFlag, ["age =>"])

      case default
        call raiseError("'" // trim(recordFlag) //"' is an invalid record flag")
    end select
  end subroutine initializeRunWriter
end submodule RunWriterHandling


submodule (CmdOptionType) KVBoundProcedure
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: KVBoundProcedure
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `CmdOptionType` containing type-bound procedures for the
  !!  type `KeyValCmdOption`.
  ! -------------------------------------------------------------------------- !
  implicit none
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignOptionalKVVal
  !>  Assign the default value to a key-value command-line option.
  !!  This also marks the command-line option optional.
  ! -------------------------------------------------------------------------- !
  subroutine assignOptionalKVVal(cmdKeyVal, value)
    class(KeyValCmdOption), intent(inout) :: cmdKeyVal
      !! Command-line key-value options.
    character(len=*),       intent(in)    :: value
      !! Default value to be assigned to `value` attribute of `self`.

    if (cmdKeyVal % isOptional) then
      print "(3a)", "***ERROR. Cannot assign '", trim(cmdKeyVal % command), &
          "' its default value again."
      stop
    else
      cmdKeyVal % hasValue = .true.
      cmdKeyVal % isOptional = .true.
      cmdKeyVal % value = value
    end if
  end subroutine assignOptionalKVVal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setValueMsg
  !>  Set the tag or message of the value. This will appear in the right-hand
  !!  side of the key-value command-line option in the help message.
  ! -------------------------------------------------------------------------- !
  subroutine setValueMsg(cmdKeyVal, valueMsg)
    class(KeyValCmdOption), intent(inout) :: cmdKeyVal
      !! Command-line key-value options.
    character(len=*),       intent(in)    :: valueMsg
      !! Short description of the value of `cmdKeyVal` option.

    cmdKeyVal % valueMsg = valueMsg
  end subroutine setValueMsg
end submodule KVBoundProcedure

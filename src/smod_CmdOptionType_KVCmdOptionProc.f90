
submodule (CmdOptionType) KVBoundProcedure
  implicit none
contains

  ! -------------------------------------------------------------------------- !
  ! FUNCTION: KVtype_getValue
  !>  Get the value of the provided key-value command-line option.
  ! -------------------------------------------------------------------------- !
  pure integer function KVtype_getValue(self)
    class(KeyValCmdOption), intent(in) :: self

    KVtype_getValue = self % value
  end function KVtype_getValue


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: assignOptionalKVVal
  !>  Assign the default value to a key-value command-line option.
  !!  This also marks the command-line option optional.
  ! -------------------------------------------------------------------------- !
  subroutine assignOptionalKVVal(cmdKeyVal, value)
    class(KeyValCmdOption), intent(inout) :: cmdKeyVal
    integer,                intent(in)    :: value

    if (cmdKeyVal % isOptional) then
      print "(3a)", "***ERROR. Cannot assign '", trim(cmdKeyVal % command), &
          "' its default value again."
      stop
    end if

    cmdKeyVal % hasValue = .true.
    cmdKeyVal % isOptional = .true.
    cmdKeyVal % value = value
  end subroutine assignOptionalKVVal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: setValueMsg
  !>  Set the tag or message of the value. This will appear in the right-hand
  !!  side of the key-value command-line option in the help message.
  ! -------------------------------------------------------------------------- !
  subroutine setValueMsg(cmdKeyVal, valueMsg)
    class(KeyValCmdOption), intent(inout) :: cmdKeyVal
    character(len=*),       intent(in)    :: valueMsg

    cmdKeyVal % valueMsg = valueMsg
  end subroutine setValueMsg
end submodule KVBoundProcedure

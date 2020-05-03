submodule (Parameters) ReadExtFile
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: ReadExtFile
  ! -------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `Parameters` containing procedures for reading files of
  !!  model and program parameters and "Verhulst weight".
  ! -------------------------------------------------------------------------- !
  implicit none

  ! -------------------------------------------------------------------------- !
  ! Parameter keys. NOTE: Padded with spaces to accept initializer.
  character(len=*), parameter :: PARAM_KEYS(*) = &
    ["L          ", &
     "T          ", &
     "B          ", &
     "M          ", &
     "R          ", &
     "R_max      ", &
     "K          ", &
     "N0         ", &
     "mttn_count ", &
     "t_max      ", &
     "sample_size", &
     "rec_flag   ", &
     "rng        ", &
     "seed       ", &
     "v_weight   " ]
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readDefaultParamVal
  !>  Read default parameter values from an external file.
  ! -------------------------------------------------------------------------- !
  subroutine readDefaultParamVal()
    type(ParamFileParser) :: paramReader

    ! Temporary character for `PROG_REC_FLAG`.
    character(len=:), allocatable :: tempRecFlag

    ! Check the existence of the file first.
    call checkParamFilePath()

    ! Read parameter file.
    call paramReader % init(FILE_PARAM_LIST)
    call paramReader % readFile()

    ! Assign scalar parameters one-by-one.
    call paramReader % getValue(PARAM_KEYS(1),  MODEL_L)
    call paramReader % getValue(PARAM_KEYS(2),  MODEL_T)
    call paramReader % getValue(PARAM_KEYS(3),  MODEL_B)
    call paramReader % getValue(PARAM_KEYS(4),  MODEL_M)
    call paramReader % getValue(PARAM_KEYS(5),  MODEL_R)
    call paramReader % getValue(PARAM_KEYS(6),  MODEL_R_MAX)
    call paramReader % getValue(PARAM_KEYS(7),  MODEL_K)
    call paramReader % getValue(PARAM_KEYS(8),  MODEL_START_POP_SIZE)
    call paramReader % getValue(PARAM_KEYS(9),  MODEL_MTTN_COUNT)
    call paramReader % getValue(PARAM_KEYS(10), MODEL_TIME_STEPS)
    call paramReader % getValue(PARAM_KEYS(11), PROG_SAMPLE_SIZE)
    call paramReader % getValue(PARAM_KEYS(12), tempRecFlag)
    call paramReader % getValue(PARAM_KEYS(13), PROG_RNG)
    call paramReader % getValue(PARAM_KEYS(14), PROG_RNG_SEED)

    ! Assign the temporary character.
    PROG_REC_FLAG = tempRecFlag

    ! Assign array parameter.
    allocate(MODEL_V_WEIGHT(MODEL_L))
    MODEL_V_WEIGHT(:) = -1
    call paramReader % getValue(PARAM_KEYS(15), MODEL_L, MODEL_V_WEIGHT)

    ! Free temporary character variable.
    deallocate(tempRecFlag)
  end subroutine readDefaultParamVal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkParamFilePath
  !>  Check the existence of the parameter listing file `FILE_PARAM_LIST`.
  ! -------------------------------------------------------------------------- !
  subroutine checkParamFilePath()
    logical   :: extant

    inquire(file=FILE_PARAM_LIST, exist=extant)
    if (.not. extant) then
      call raiseError( &
        "The parameter listing file '" &
        // trim(FILE_PARAM_LIST) // "' cannot be read" &
        )
    end if
  end subroutine checkParamFilePath
end submodule ReadExtFile

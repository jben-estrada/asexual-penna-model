submodule (Parameters) ReadExtFile
  ! -------------------------------------------------------------------------- !
  ! SUBMODULE: ReadExtFile
  ! -------------------------------------------------------------------------- !
  ! AUTHOR: John Benedick A. Estrada
  !--------------------------------------------------------------------------- !
  ! DESCRIPTION:
  !>  Submodule of `Parameters` containing procedures for reading files of
  !!  model and program parameters and "Verhulst weight".
  ! -------------------------------------------------------------------------- !
  implicit none

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
    !! Parameter keys in the parameter listing file.
contains


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: readDefaultParamVal
  !>  Read default parameter values from an external file.
  ! -------------------------------------------------------------------------- !
  subroutine readDefaultParamVal()
    type(ParamFileParser) :: paramReader

    ! Temporary character for `PROG_REC_FLAG`.
    character(len=:), allocatable :: tempRecFlag

    ! Array of getter statuses.
    integer :: getStats(size(PARAM_KEYS))

    ! Check the existence of the file first.
    call checkParamFilePath()

    ! Read parameter file.
    call paramReader % init(FILE_PARAM_LIST)
    call paramReader % readFile()

    ! Assign scalar parameters one-by-one.
    call paramReader % getValue(PARAM_KEYS(1),  MODEL_L, getStats(1))
    call paramReader % getValue(PARAM_KEYS(2),  MODEL_T, getStats(2))
    call paramReader % getValue(PARAM_KEYS(3),  MODEL_B, getStats(3))
    call paramReader % getValue(PARAM_KEYS(4),  MODEL_M, getStats(4))
    call paramReader % getValue(PARAM_KEYS(5),  MODEL_R, getStats(5))
    call paramReader % getValue(PARAM_KEYS(6),  MODEL_R_MAX, getStats(6))
    call paramReader % getValue(PARAM_KEYS(7),  MODEL_K, getStats(7))
    call paramReader % getValue(PARAM_KEYS(8),  MODEL_START_POP_SIZE, &
        getStats(8))
    call paramReader % getValue(PARAM_KEYS(9),  MODEL_MTTN_COUNT, getStats(9))
    call paramReader % getValue(PARAM_KEYS(10), MODEL_TIME_STEPS, getStats(10))
    call paramReader % getValue(PARAM_KEYS(11), PROG_SAMPLE_SIZE, getStats(11))
    call paramReader % getValue(PARAM_KEYS(12), tempRecFlag, getStats(12))
    call paramReader % getValue(PARAM_KEYS(13), PROG_RNG, getStats(13))
    call paramReader % getValue(PARAM_KEYS(14), PROG_RNG_SEED, getStats(14))

    ! Assign the temporary character.
    PROG_REC_FLAG = tempRecFlag

    ! Assign array parameter.
    allocate(MODEL_V_WEIGHT(MODEL_L))
    MODEL_V_WEIGHT(:) = -1  ! Assign some placeholder value.

    call paramReader % getValue(PARAM_KEYS(15), MODEL_V_WEIGHT, getStats(15))

    ! Check if all the getters succeeded in obtaining the parameters.
    call checkParamExistence(getStats)

    ! Free all local allocatable variables.
    deallocate(tempRecFlag)
    call paramReader % free()
  end subroutine readDefaultParamVal


  ! -------------------------------------------------------------------------- !
  ! SUBROUTINE: checkParamExistence
  !>  Check the existence of the individual parameters in the parameter listing
  !!  file `FILE_PARAM_LIST`.
  ! -------------------------------------------------------------------------- !
  subroutine checkParamExistence(getStats)
    integer, intent(in) :: getStats(size(PARAM_KEYS))
  
    character(len=:), allocatable :: missingParams
    integer :: i
    logical :: atFirstParam

    if (all(getStats(:) == 0)) return

    ! Initialize local variables.
    allocate(character(len=0) :: missingParams)
    atFirstParam = .true.

    ! Find the missing parameters for them to be printed in the error msg.
    do i = lbound(getStats, 1), ubound(getStats, 1)
      if (getStats(i) /= 0) then
        if (atFirstParam) then
          missingParams = trim(PARAM_KEYS(i))
          atFirstParam = .false.
        else
          missingParams = missingParams // ", " // trim(PARAM_KEYS(i))
        end if
      end if
    end do

    call raiseError(new_line("") // "  " // &
      "In the parameter listing file '" &
      // trim(FILE_PARAM_LIST) // "', " // &
      new_line("") // "  the following parameters are missing: " &
      // missingParams &
      )
  end subroutine checkParamExistence


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

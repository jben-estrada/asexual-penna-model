program main
    use Model
    use PersonMod
    use Pop
    use UpdateArray
    use SaveFormat
    use iso_fortran_env, only: real64
    implicit none

    integer :: max_loop, sample_num, starting_pop_   ! cmd arguments
    real (kind=real64) :: time_duration

    ! Get command line arguments
    call get_cmd_args(max_loop, sample_num, starting_pop_)

    ! Run the Penna model
    print "(a,/a)", "==========", "Penna model simulation"
    call print_params(max_loop, sample_num, starting_pop_, .true.)
    call multiple_run(max_loop, starting_pop_, sample_num, time_duration)

    print "(a, f10.3, a)", "Average time duration:", time_duration, " ms"

    contains
    subroutine run(max_timestep, starting_pop_size)
        integer, intent(in) :: max_timestep, starting_pop_size
        integer :: pop_size,       &  ! Population size
                   step,           &  ! Time step
                   index,          &  ! Index of population array
                   error              ! Integer representation of alloc error
        logical, allocatable :: mask(:)
        type(Person), allocatable :: curr_pop(:), & ! Current population array
                                     next_pop(:), &    ! Next population array
                                     masked_pop(:), &
                                     new_born_pop(:)
        call generate_population(curr_pop, starting_pop_size)
        next_pop = curr_pop

        ! Initialize writer
        !   .txt file for Population
        call initialize_writer(pop_filename, pop_unit, pop_position)

        ! === Main loop ===
        do step = 1, max_timestep
            ! Initialize variables 
            pop_size = size(curr_pop)
            allocate(mask(size(curr_pop)), new_born_pop(0), stat=error)
            call alloc_check(error)

            ! Check if the population hit the limit
            if (pop_size > MODEL_K) then
                print *, "The population exceeded the carrying capacity"
                stop
            end if

            ! === Evaluate each individuals ===
            do index = 1, pop_size
                call step_age(next_pop(index), new_born_pop, pop_size)

                ! Check death of individual
                if (next_pop(index)%death_index == ALIVE) then
                    mask(index) = .true.
                else
                    mask(index) = .false.
                end if
            end do
            ! =================================
            ! Recording population
            write(pop_unit, pop_format) pop_size

            ! Remove dead individuals
            masked_pop = pack(next_pop, mask)

            ! Add new born individuals
            call array_insert_range(masked_pop, new_born_pop, 1)

            ! Reset arrays
            deallocate(curr_pop, next_pop, mask, new_born_pop)
            call move_alloc(masked_pop, curr_pop)
            next_pop = curr_pop
        end do
        ! =================
        ! Close writers
        close(pop_unit)

        ! Deallocate arrays. Just to be sure
        deallocate(next_pop, curr_pop)
    end subroutine run


    subroutine multiple_run(max_time_step, starting_pop_size, &
            sample_size, wall_time)
        implicit none
        integer, intent(in) :: max_time_step, sample_size, starting_pop_size
        double precision, intent(out) :: wall_time
        double precision :: start_time, end_time, sum
        integer :: i

        if (sample_num == 1) then
            call single_run(max_time_step, starting_pop_size, wall_time)
            return
        end if

        sum = 0
        do i = 1, sample_size
            call cpu_time(start_time)
            call run(max_time_step, starting_pop_size)
            call cpu_time(end_time)
            sum = sum + (end_time - start_time)*1e3
        end do

        wall_time = sum/sample_size

        ! Record timing result
        call initialize_writer(time_filename, time_unit, time_position)
        write(time_unit, time_format) max_time_step, starting_pop_size, wall_time
        close(time_unit)
    end subroutine multiple_run


    subroutine single_run(max_timestep, starting_pop_size, wall_time)
        implicit none
        integer, intent(in) :: max_timestep, starting_pop_size
        double precision, intent(out) :: wall_time
        double precision :: start_time, end_time

        call cpu_time(start_time)
        call run(max_timestep, starting_pop_size)
        call cpu_time(end_time)

        wall_time = (end_time - start_time)*1e3

        ! Record timing result
        call initialize_writer(time_filename, time_unit, time_position)
        write(time_unit, time_format) max_timestep, starting_pop_size, wall_time
        close(time_unit)
    end subroutine single_run


    subroutine print_params(max_timestep, sample_size, starting_pop_size, is_bare)
        implicit none
        integer, intent(in) :: max_timestep, sample_size, starting_pop_size
        logical, intent(in) :: is_bare
        integer :: i

        if (.not.is_bare) then
            print "(*(a))", ("-", i = 1, 16)
            print "((a10, a6/), 7(a10, i6/), *(a))", "PARAMATER|", "VALUE", &
                "L |", MODEL_L, &
                "T |", MODEL_T, &
                "B |", MODEL_B, &
                "M |", MODEL_M, &
                "R |", MODEL_R, &
                "R MAX |", MODEL_R_MAX, &
                "K |", MODEL_K, &
                ("-", i = 1, 16)
            print "(a)", "> Verhulst factor weights per age:"
            print "(*(t10, 5(a, i3, a, f4.1, a)/))", ("(", i, ":", &
                MODEL_VERHULST_W(i), ") ", i = 1, MODEL_L)
        end if
        print "(a, i6)", "> Total time step:", max_timestep
        print "(a, i6)", "> Number of samples:", sample_size
        print "(a, i6)", "> Starting population count:", starting_pop_size
    end subroutine print_params


    subroutine get_cmd_args(max_timestep, sample_size, starting_pop_size)
        implicit none
        integer ,intent(out) :: max_timestep, sample_size, starting_pop_size
        integer :: i, cmd_int, cmd_error
        character(32) :: cmd_arg

        ! Default values for cmd arguments
        max_timestep = 100
        sample_size = 1
        starting_pop_size = MODEL_N_START

        do i = 1, 3
            call get_command_argument(i, cmd_arg, status=cmd_error)
            if (cmd_error /= 0) cycle
    
            read(cmd_arg, *) cmd_int
            select case (i)
            case (1)
                max_timestep = cmd_int
            case (2)
                sample_size = cmd_int
            case (3)
                starting_pop_size = cmd_int
            end select
        end do
    end subroutine get_cmd_args
end program main

module PersonMod
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
    ! Module containing the type definition of `Person`
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

    use Model, only: MODEL_L
    use Flag, only: ALIVE

    ! `Person` type
    type Person
        integer :: age, mutation_count, death_index
        logical :: genome(MODEL_L)
        real :: bad_gene_ratio
    end type Person

end module PersonMod


module Pop
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
    ! Module containing anything related to population and procedures
    ! related to evaluation of `Person` objects.
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

    use Gene, only: GENE_UNHEALTHY, GENE_HEALTHY, Gene_mutated_gene
    use UpdateArray, only: array_insert_range, array_remove
    use PersonMod, only: Person
    use RandInd, only: generate_indices
    use Model
    use Flag    
    implicit none

    private :: check_birth,             &
               reproduce,               &
               killed_by_mutation,      &
               killed_by_verhulst,      &
               killed_by_age,           &
               get_bad_gene_ratio,      &
               initialize_scalar_attrs, &
               initialize_healthy_indiv

    contains
    ! === INDIVIDUAL EVALUATION ==============================================
    subroutine step_age(indiv, pop_array, pop_size)
        implicit none
        integer, intent(in) :: pop_size
        type(Person), allocatable, intent(inout) :: pop_array(:)
        type(Person), intent(inout) :: indiv

        ! Increment age
        indiv%age = indiv%age + 1

        ! Death check
        if (killed_by_mutation(indiv)) then
            return
        else if (killed_by_verhulst(indiv, pop_size)) then
            return
        else if (killed_by_age(indiv)) then
            return
        end if

        call check_birth(indiv, pop_array)
    end subroutine step_age


    function killed_by_mutation(indiv) result(is_dead)
        type(Person) :: indiv
        logical :: is_dead
        is_dead = .false.

        if (indiv%genome(indiv%age) .eqv. GENE_UNHEALTHY) then
            indiv%mutation_count = indiv%mutation_count + 1
        end if
        if (indiv%mutation_count >= MODEL_T) then
            indiv%death_index = DEAD_MUTATION
            is_dead = .true.
        end if
    end function killed_by_mutation


    function killed_by_verhulst(indiv, pop_size) result(is_dead)
        type(Person), intent(inout) :: indiv
        integer, intent(in) :: pop_size
        real :: v_weight, verhulst_factor, random
        logical :: is_dead
        is_dead = .false.

        v_weight = MODEL_VERHULST_W(indiv%age)
        verhulst_factor = 1 - (pop_size/MODEL_K)*v_weight
        call random_number(random)

        if (random > verhulst_factor) then
            indiv%death_index = DEAD_VERHULST
            is_dead = .true.
        end if
    end function killed_by_verhulst


    function killed_by_age(indiv) result(is_dead)
        type(Person), intent(inout) :: indiv
        logical :: is_dead
        is_dead = .false.

        if (indiv%age >= MODEL_L) then
            ! print *, "dead"
            indiv%death_index = DEAD_OLD_AGE
            is_dead = .true.
        end if
    end function killed_by_age


    subroutine check_birth(indiv, pop_array)
        implicit none
        type(Person), allocatable, intent(inout) :: pop_array(:)
        type(Person), intent(inout) :: indiv
    
        if (MODEL_R <= indiv%age .and. indiv%age <= MODEL_R_MAX) then
            call reproduce(pop_array, indiv%genome)
        end if
    end subroutine check_birth


    subroutine reproduce(pop_array, genome)
        implicit none
        type(Person), allocatable, intent(inout):: pop_array(:)
        logical, intent(inout) :: genome(:)

        type(Person) :: offsprings(MODEL_B)
        integer :: i

        do i = 1, MODEL_B
            call initialize_indiv(offsprings(i), genome)
        end do

        call array_insert_range(pop_array, offsprings, 1)
    end subroutine reproduce
    ! ========================================================================

    ! === INITIALIZE `PERSON` TYPE ===========================================
    subroutine initialize_indiv(indiv, genome)
        implicit none
        logical, intent(in) :: genome(:)
        type(Person), intent(inout) :: indiv
        integer :: mutations(MODEL_M), i

        call generate_indices(1, MODEL_L, mutations)

        ! Copy genome from the parent and apply mutations
        ! if there are any.
        do i = 1, size(genome)
            if (any(mutations == i)) then
                indiv%genome(i) = Gene_mutated_gene()
            else
                indiv%genome(i) = genome(i)
            end if
        end do

        call initialize_scalar_attrs(indiv)
    end subroutine initialize_indiv


    subroutine initialize_healthy_indiv(indiv)
        implicit none
        type(Person), intent(inout) :: indiv
        integer :: i
        
        indiv%genome = [(GENE_HEALTHY, i = 1, MODEL_L)]
        call initialize_scalar_attrs(indiv)
    end subroutine initialize_healthy_indiv


    subroutine initialize_scalar_attrs(indiv)
        implicit none
        type(Person), intent(inout) :: indiv

        indiv%age = 0
        indiv%mutation_count = 0
        indiv%death_index = ALIVE
        indiv%bad_gene_ratio = get_bad_gene_ratio(indiv%genome)
    end subroutine initialize_scalar_attrs


    function get_bad_gene_ratio(genome) result(ratio)
        logical, intent(in) :: genome(:)
        integer :: i, L
        real :: ratio
        ratio = 0.

        L = size(genome)
        do i = 1, L
            if (genome(i) .eqv. GENE_UNHEALTHY) then
                ratio = ratio + 1/L
            end if
        end do
    end function get_bad_gene_ratio
    ! ========================================================================

    ! === GENERATE POPULATION ================================================
    subroutine generate_population(population, starting_pop)
        type(Person), allocatable, intent(inout) :: population(:)
        integer, intent(in) :: starting_pop
        integer :: i
        allocate(population(starting_pop))

        do i = 1, starting_pop
            call initialize_healthy_indiv(population(i))
        end do
    end subroutine generate_population
end module Pop

! program test
!     use PersonMod
!     use Gene
!     use Pop, only: initialize_indiv
!     implicit none
!     integer :: i
!     type(Person) :: a, b

!     call initialize_indiv(a, [(GENE_HEALTHY, i = 1, MODEL_L)])
!     call initialize_indiv(b, [(GENE_HEALTHY, i = 1, MODEL_L)])

!     print *, a
!     print *, b

!     call increment_mutation(a)

!     print *, a
!     print *, b

!     contains
!     subroutine increment_mutation(indiv)
!         implicit none
!         type(Person), intent(out) :: indiv
!         indiv%mutation_count = indiv%mutation_count + 1
!     end subroutine increment_mutation
! end program test
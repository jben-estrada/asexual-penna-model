program test
    implicit none
    integer :: partition = 20, total = 13, bit_length
    integer :: i, j, k
    character :: bit = ">"
    character, allocatable :: first_part(:), last_part(:)

    do i = 1, total
        call sleep(1)  ! NOTE: GNU extension. Nonstandard
        bit_length = int(i*partition/total)
        first_part = [(bit, k = 1, bit_length)]
        last_part = [(" ", k = partition - 1, bit_length, -1)]
        write(*, "(*(a))", advance="no") (char(8), j = 1, partition + 7)
        write(*, "(*(a))", advance="no") "[", first_part, last_part, "]"
        write(*, "(i4, a)", advance="no") bit_length*100/partition, "%"
    end do
end program test
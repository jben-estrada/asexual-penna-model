program test
  implicit none
  integer :: a
  a = 100
  print "(b0)", a

  call read(a)

  a = a + shiftl(1, 3)

  print "(b0)", a

  call read(a)
  
contains

  subroutine read(number)
    implicit none
    integer, intent(in) :: number
    integer :: i
    do i = 0, floor(log(real(number))/log(2.))
      print *, i, iand(shiftr(number, i), 1)
    end do
  end subroutine read
  
end program test
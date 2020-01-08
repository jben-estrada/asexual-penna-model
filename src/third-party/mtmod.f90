!____________________________________________________________________________
! A C-program for MT19937: Real number version
!   genrand() generates one pseudorandom real number (double)
! which is uniformly distributed on [0,1]-interval, for each
! call. sgenrand(seed) set initial values to the working area
! of 624 words. Before genrand(), sgenrand(seed) must be
! called once. (seed is any 32-bit integer except for 0).
! Integer generator is obtained by modifying two lines.
!   Coded by Takuji Nishimura, considering the suggestions by
! Topher Cooper and Marc Rieffel in July-Aug. 1997.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Library General Public
! License as published by the Free Software Foundation; either
! version 2 of the License, or (at your option) any later
! version.
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! See the GNU Library General Public License for more details.
! You should have received a copy of the GNU Library General
! Public License along with this library; if not, write to the
! Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
! 02111-1307  USA
!
! Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.
! When you use this, send an email to: matumoto@math.keio.ac.jp
! with an appropriate reference to your work.
!
!***********************************************************************
! Fortran translation by Hiroshi Takano.  Jan. 13, 1999.
!
!   genrand()      -> double precision function grnd()
!   sgenrand(seed) -> subroutine sgrnd(seed)
!                     integer seed
!
! This program uses the following non-standard intrinsics.
!   ishft(i,n): If n>0, shifts bits in i by n positions to left.
!               If n<0, shifts bits in i by n positions to right.
!   iand (i,j): Performs logical AND on corresponding bits of i and j.
!   ior  (i,j): Performs inclusive OR on corresponding bits of i and j.
!   ieor (i,j): Performs exclusive OR on corresponding bits of i and j.
!
!***********************************************************************
! Fortran version rewritten as an F90 module and mt state saving and getting
! subroutines added by Richard Woloshyn. (rwww@triumf.ca). June 30, 1999
!
!***********************************************************************
! The `mtmod` module was changed to remove implicitly declared variables;
! all of them are now explicitly declared. The driving program was also
! removed to be able to use in other programs.
!
! In regard to the non-standard instrinsic procedures stated above,
! this module do seem to compile in `gfortran 9.2.1` with the `-std=f2008`
! flag. Meaning, their usage here could be within standard. However, one
! should still exercise caution as, in some compilers such as in GCC, these
! instrinsics are overloaded with non-standard specific procedures.
! 
! The changes were made by John Benedick Estrada (jestrada@nip.upd.edu.ph)
! in December 2019 - January 2020.
! 

 module mtmod
    private
! Default seed
    integer, parameter :: defaultsd = 4357
! Period parameters
    integer, parameter :: N = 624, N1 = N + 1

! the array for the state vector
    integer, save :: mt(0: N-1)
    integer, save :: mti = N1
    
! Overload procedures for saving and getting mt state
    interface mtsave
      module procedure mtsavef
      module procedure mtsaveu
    end interface
    interface mtget
      module procedure mtgetf
      module procedure mtgetu
    end interface

    public :: mtsave
    public :: mtget
    public :: grnd
    public :: sgrnd
 contains

!Initialization subroutine
  subroutine sgrnd(seed)
    implicit none
!
!      setting initial seeds to mt[N] using
!      the generator Line 25 of Table 1 in
!      [KNUTH 1981, The Art of Computer Programming
!         Vol. 2 (2nd Ed.), pp102]
!
    integer, intent(in) :: seed

    mt(0) = iand(seed,-1)
    do mti = 1, N-1
      mt(mti) = iand(69069 * mt(mti-1),-1)
    end do
!
    return
  end subroutine sgrnd

!Random number generator
  real(8) function grnd()
    implicit none

! Period parameters
    integer, parameter :: M = 397, MATA  = -1727483681
!                                    constant vector a
    integer, parameter :: LMASK =  2147483647
!                                    least significant r bits
    integer, parameter :: UMASK = -LMASK - 1
!                                    most significant w-r bits
! Tempering parameters
    integer, parameter :: TMASKB= -1658038656, TMASKC= -272236544

! Temporary variables
    integer :: k, y
!
    integer, save :: mag01(0:1) = (/0, MATA/)
!                        mag01(x) = x * MATA for x=0,1

    if (mti >= N) then
!                       generate N words at one time
      if (mti == N+1) then
!                            if sgrnd() has not been called,
        call sgrnd( defaultsd )
!                              a default initial seed is used
      end if

      do k = 0, N-M-1
          y = ior(iand(mt(k), UMASK), iand(mt(k+1), LMASK))
          mt(k) = ieor(ieor(mt(k+M), ishft(y,-1)), mag01(iand(y,1)))
      end do

      do k = N-M, N-2
          y = ior(iand(mt(k), UMASK), iand(mt(k+1), LMASK))
          mt(k) = ieor(ieor(mt(k + (M - N)), ishft(y, -1)), mag01(iand(y, 1)))
      end do

      y = ior(iand(mt(N-1), UMASK), iand(mt(0), LMASK))
      mt(N-1) = ieor(ieor(mt(M-1), ishft(y, -1)), mag01(iand(y, 1)))
      mti = 0
    end if

    y=mt(mti)
    mti = mti + 1
    y = ieor(y, ishft(y, -11))
    y = ieor(y, iand(ishft(y, 7), TMASKB))
    y = ieor(y, iand(ishft(y, 15), TMASKC))
    y = ieor(y, ishft(y, -18))

    if (y  < 0) then
      grnd=(dble(y) + 2.0d0**32)/(2.0d0**32-1.0d0)
    else
      grnd=dble(y)/(2.0d0**32-1.0d0)
    end if

    return
  end function grnd

!State saving subroutines.
! Usage:  call mtsave( file_name, format_character )
!    or   call mtsave( unit_number, format_character )
! where   format_character = 'u' or 'U' will save in unformatted form, otherwise
!         state information will be written in formatted form.
  subroutine mtsavef( fname, forma )
    implicit none
!NOTE: This subroutine APPENDS to the end of the file "fname".

    character(*), intent(in) :: fname
    character, intent(in)    :: forma

    select case (forma)
      case('u','U')
       open(unit=10, file=trim(fname), status='UNKNOWN', form='UNFORMATTED', &
            position='APPEND')
       write(10)mti
       write(10)mt

      case default
       open(unit=10, file=trim(fname), status='UNKNOWN', form='FORMATTED', &
            position='APPEND')
       write(10,*)mti
       write(10,*)mt

    end select
    close(10)

    return
  end subroutine mtsavef

  subroutine mtsaveu( unum, forma )
    implicit none

    integer, intent(in)    :: unum
    character, intent(in)  :: forma

    select case (forma)
      case('u','U')
       write(unum)mti
       write(unum)mt

      case default
       write(unum,*)mti
       write(unum,*)mt

      end select

    return
  end subroutine mtsaveu

!State getting subroutines.
! Usage:  call mtget( file_name, format_character )
!    or   call mtget( unit_number, format_character )
! where   format_character = 'u' or 'U' will read in unformatted form, otherwise
!         state information will be read in formatted form.
  subroutine mtgetf( fname, forma )
    implicit none

    character(*), intent(in) :: fname
    character, intent(in)    :: forma

    select case (forma)
      case('u','U')
       open(unit=10,file=trim(fname),status='OLD',form='UNFORMATTED')
       read(10)mti
       read(10)mt

      case default
       open(unit=10,file=trim(fname),status='OLD',form='FORMATTED')
       read(10,*)mti
       read(10,*)mt

    end select
    close(10)

    return
  end subroutine mtgetf

  subroutine mtgetu( unum, forma )
    implicit none
    
    integer, intent(in)    :: unum
    character, intent(in)  :: forma

    select case (forma)
      case('u','U')
       read(unum)mti
       read(unum)mt

      case default
       read(unum,*)mti
       read(unum,*)mt

      end select

    return
  end subroutine mtgetu

 end module mtmod
 
      program subroutines

      implicit none

      double precision :: rad1, rad2, vol1, vol2
      character :: option

      do
        print *, 'please enter rad and rad2'
        read *, rad1, rad2

        call volume(rad1, vol1)
        call volume(rad2, vol2)

        print 10, abs(vol1 - vol2)
   10   format ('The diference is ', f6.1)

        print *
        print *, 'Do you whish calculate again?'
        read *, option
        print *

        if (option .ne. 'Y' .and. option .ne. 'y') stop
      end do

      end program subroutines


      subroutine volume(rad, vol)

      implicit none
    
      double precision :: rad, vol, pi
      
      pi = 4.0*atan(1.0)
      vol = 4./3.*pi*rad**3

      end subroutine volume

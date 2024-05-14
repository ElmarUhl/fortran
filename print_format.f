      program print_format

      implicit none

      double precision, dimension(4) :: matrix
      integer :: i

      do i=1, 4
        matrix(i) = cos(0.1*i)
      end do

      print *, 'matrix'
      print *, 'This is an unformated print'
      print *, matrix
      print *

      print *, 'This is a formated print'
      print 10, matrix
  10  format(2f20.4)

      end program print_format
      program division

      implicit none

      integer :: x, y, answer
      real :: a, b, c

      print *, 'Type two numbers: '
      read *, x, y
      answer = x/y
      print *, 'The division by two integer numbers is ', answer 

      print *, 'Type two numbers: '
      read *, a, b
      c = a/b
      print *, 'The result of dividing two real numbers is ', c
      
      answer = x/y
      print *, 'Dividing x = ', x, ' per a = ', a, ' is ', answer
      c = x/a
      print *, 'Dividing x = ', x, ' per a = ', a, ' is ', c

      end
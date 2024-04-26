      program sum

      implicit none

      real :: x, y, answer

      print *, 'Type two numbers: '
      read *, x, y
      print *, 'You typed x = ', x, 'and y = ', y
      answer = x + y
      print *, 'The sum of x and y is ', answer

      end program sum
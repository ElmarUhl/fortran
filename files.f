      program files

        implicit none

        real :: x, y, z

c       read file
        open(10, file='test.txt')
        read (10,*) x, y, z
        print *, x, y, z

        open(20, file='text2.txt')
        write (20,*) x, y, z
        print *, 'Your data was written'
        
      end program files
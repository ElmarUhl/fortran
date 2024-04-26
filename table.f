       program table

        implicit none

        real :: x, y, z

        open(10, file='mytable.txt')
        print *, '   x     y     z'
        write(10,*) '    x       y       z'

        do x = 1, 4
            do y = 1, 6, 0.5
                z = x*y
                print *, x, y, z
                write (10,*) x, y, z
            end do
        end do

        end program table
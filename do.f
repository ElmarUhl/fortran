      program do_loop

        implicit none

        integer :: i

        do i = 0, 10
            print *, i
        end do

        do i = 10, -10, -2
            print *, i
        end do

        i = 0
        do while (i < 100)
            print *, i
            i = i + 1
        end do

        end program do_loop
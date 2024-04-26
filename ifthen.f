      program ifthen
        implicit none;

        real :: x, y, answer
        integer :: choice

        x = 12
        y = 3

        print *, 'Choose an option'
        print *, '1 - sum'
        print *, '2 - multiplication'
        print *, '3 - division'

        read *, choice

        if (choice == 1) then
            answer = x + y
            print *, answer
        else if (choice == 2) then
            answer = x*y
            print *, answer
        else if (choice == 3) then
            answer = x/y
            print *, answer
        else
            print *, 'Invalid option'
        end if
        
      end program ifthen
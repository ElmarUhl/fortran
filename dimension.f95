program dimension

    implicit none

    real, allocatable, dimension(:) :: x
    integer :: elements

    elements = 3

    allocate(x(elements))

    x(1) = 1.0
    x(2) = 2.0
    x(3) = 3.0

    print *, x

    deallocate(x)
    
end program dimension
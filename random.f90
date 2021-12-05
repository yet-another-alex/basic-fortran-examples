!!
! basic example for random number generation of integer type
! for real, use random_number
!!
program random_number
    implicit none

    ! constants
    integer :: max_number = 1000

    ! declaring variables
    integer :: sorting_array(1000)
    integer :: i

    ! fill with pseudo random numbers of type integer (otherwise random_number is better)
    do i = 1, size(sorting_array)
        sorting_array(i) = modulo(irand(), max_number+1)
    end do

    ! print the array
    print *, sorting_array
end program
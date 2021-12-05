!!
! example for calculating Pi.
! Change data type of "sum" for more or less accurate calculation results
! Change "counter_max" for iterations, less means less accurate, more means more accurate but also longer time taken
!!
program pi
    implicit none

    ! declaring variables
    real :: denominator
    real*16 :: sum              ! change data type to cut down runtime but also accuracy
    integer :: counter
    integer :: counter_max

    ! initialize variables
    counter = 0
    counter_max = 100000000     ! it's really interesting to see what happens if this value gets low
    denominator = 1.0
    sum = 0.0

    ! calculation loop
    do
        ! separate even and uneven runs
        if(modulo(counter, 2) .eq. 0) then
            ! positive
            sum = sum + (4 / denominator)
        else
            ! negative
            sum = sum - (4 / denominator)
        end if

        ! increment denominator
        denominator = denominator + 2

        ! exit if counter is at 0
        if(counter > counter_max) then
            exit
        end if
        ! increment counter
        counter = counter + 1
    end do

    ! print out number of iterations and finally the sum
    print *, 'iterations: ', counter_max
    print *, 'calculated pi: ', sum

end program
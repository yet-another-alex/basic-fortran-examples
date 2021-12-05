!!
! basic examples of fibonacci calculation
! takes user input to decide on the max calculation loops
! will also stop, if the fibonacci-number has reached infinity
! to calculate more numbers, adjust the data type.
!!
program fibonacci
    implicit none

    ! declaring variables
    integer :: counter
    integer :: maxloops
    real :: fibo_old
    real :: fibo_new
    real :: fibo_temp

    ! output info
    print *, 'Calculating Fibonacci Numbers ..'

    ! read number of loops
    print *, 'Enter the number of iterations:'
    read(*, *) maxloops

    ! assign starting values
    fibo_old = 0
    fibo_new = 1
    counter = 1

    ! calculation loop
    do
        ! calculate fibonacci number
        fibo_temp = fibo_new + fibo_old
        fibo_old = fibo_new
        fibo_new = fibo_temp
        counter = counter + 1
        maxloops = maxloops - 1
    
        ! output to screen
        print *, "Loop: ", counter
        print *, "Remaining: ", maxloops
        print *, 'Fibonacci number: ', fibo_new 

        ! check if we reached infinity and finish useless calculation
        if(fibo_new > huge(fibo_new)) then
            print *, "reached infinity after iteration: ", counter-1
            exit
        end if

        ! non infinity exit condition, if user requested loops are over
        if(maxloops<2) exit
    end do

end program fibonacci
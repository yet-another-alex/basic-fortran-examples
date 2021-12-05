!!
! basic file input/output example
! fills two arrays with random (real) numbers and prints them in a file
! reads the file back again but swaps the array order around
! if the file exists, it will be deleted first
!!
program fileio
    implicit none
    ! declare variables
    real :: col1(2500), col2(2500)
    integer :: i
    logical :: exists

    ! fill arrays with random real numbers
    call random_number(col1)
    call random_number(col2)

    ! check if the file exists
    inquire(file='data.csv', exist=exists)
    ! delete if it exists
    if (exists) then
        ! can't be outright deleted but can be deleted upon closing
        open(3, file='data.csv', status='old')
        close(3, status='delete')
    end if

    ! print to file as comma separated values
    open(1, file = 'data.csv', status = 'new')
    do i = 1, 2500
        write(1, '(3g0)') col1(i), ',', col2(i)
    end do
    ! close the file again
    close(1)

    ! read from file to array in reverse order
    open(2, file = 'data.csv', status = 'old')
    do i = 1, 2500
        read(2, *) col2(i), col1(i)
        ! print *, col2(i), col1(i)         ! console readable output, uncomment for verification
    end do
    ! close the file again
    close(2)
end program
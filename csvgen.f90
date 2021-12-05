!!
! basic program to generate large amounts of random integer-data in 3 columns into csv files
! for more columns, add more "col" arrays
! for more lines, adjust the size of those arrays
!!
program csvgen
    implicit none
    ! constants
    character(len=20) :: filename = 'datagen.csv'
    ! declaring variables
    integer :: col1(100000), col2(100000), col3(100000)        ! this number is the amount of lines being generated, 3 columns
    integer :: i

    ! fill column arrays with random numbers
    call fill_random(size(col1), 2500, col1)
    call fill_random(size(col2), 2500, col2)
    call fill_random(size(col3), 2500, col3)

    ! check the file and delete if it exists
    call check_delete(filename)

    ! open the file for writing
    open(1, file = filename, status = 'new')
    ! iterate the array
    do i = 1, size(col1)
        write(1, '(5g0)') col1(i), ',', col2(i), ',', col3(i)
    end do
    ! close the file again
    close(1)

end program

! Subroutine to check if a file exists and delete it if it does
! filename      : character, length 20, input, filename
!!
subroutine check_delete(filename)
    implicit none
    ! variables
    character(len=20), intent(in) :: filename
    logical :: exists

    ! check if the file exists
    inquire(file=filename, exist=exists)
    ! delete if it exists
    if (exists) then
        ! can't be outright deleted but can be deleted upon closing
        open(3, file=filename, status='old')
        close(3, status='delete')
    end if
end subroutine

! Subroutine to fill an integer array with random numbers
! msize     : integer, input, size of the array - this many random integers will be put into the array
! mnumber   : integer, input, maximum number - this is the maximum number a "random" number can have
! array     : integer array, input / output, array to fill with numbers
!!
subroutine fill_random(msize, mnumber, array)
    implicit none
    ! variables
    integer, intent(in) :: msize
    integer, intent(in) :: mnumber
    integer, intent(out) :: array(msize)
    integer :: i

    ! iterate through the array and fill with random integers capped at mnumber
    do i = 1, msize
        array(i) = modulo(irand(), mnumber+1)
    end do
end subroutine fill_random
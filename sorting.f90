!!
! basic sorting example
! includes random integer generation
! includes elapsed time of the sorting algorithm
! includes sorting algorithms: bubble, selection, insertion, quick
! executes the sorting algorithms in order and prints out time taken for the sorting in ms
!!
program sorting
    implicit none

    ! constants
    integer :: max_number = 1000

    ! declaring variables
    integer :: sorting_array(100000)
    !integer :: i
    integer :: start(8)
    real :: time_used

    ! fill with pseudo random numbers of type integer (otherwise random_number is better)
    call fill_random(size(sorting_array), max_number, sorting_array)
    ! save time before sorting
    call date_and_time(values=start)

    ! execute bubble sort
    call bubble_sort(size(sorting_array), sorting_array)
    ! update the time
    call elapsed_time(time_used, start)
    print *, 'bubble sort: ', time_used

    ! randomize again
    call fill_random(size(sorting_array), max_number, sorting_array)
    call date_and_time(values=start)

    ! execute selection sort
    call selection_sort(size(sorting_array), sorting_array)
    ! update the time
    call elapsed_time(time_used, start)
    print *, 'selection sort: ', time_used

    ! randomize again
    call fill_random(size(sorting_array), max_number, sorting_array)
    call date_and_time(values=start)

    ! execute insertion sort
    call insertion_sort(size(sorting_array), sorting_array)
    ! update the time
    call elapsed_time(time_used, start)
    print *, 'insertion sort: ', time_used

    ! randomize again
    call fill_random(size(sorting_array), max_number, sorting_array)
    call date_and_time(values=start)

    ! execute quick sort
    call quick_sort(size(sorting_array), sorting_array, 0, size(sorting_array))
    ! update the time
    call elapsed_time(time_used, start)
    print *, 'quick sort: ', time_used
end program

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

! Subroutine to calculate elapsed time between two calls of date_and_time()
! time_seconds  : variable for output, time in milliseconds
! start         : integer(8) - input array from a previous date_and_time()-call
!!
subroutine elapsed_time(time_mseconds, start)
    implicit none
    ! variables
    integer :: now(8), difference(8)
    integer, intent(in) :: start(8)
    real, intent(out) :: time_mseconds

    ! get current date and  time
    call date_and_time(values=now)

    ! calculate the difference in seconds
    difference = now - start
    ! 6 = minutes, 7 = seconds, 8 = milliseconds
    time_mseconds = difference(6) * 60000 + difference(7) * 1000 + difference(8)
end subroutine elapsed_time

! Bubble Sort implementation
! msize     : integer, input, size of the array
! array     : integer(msize), input / output
!!
subroutine bubble_sort(msize, array)
    implicit none
    ! variables
    integer, intent(in) :: msize
    integer, intent(inout) :: array(msize)
    logical :: swap
    integer :: i
    integer :: temp

    ! logical is initialized as F by default, set to T
    swap = .true.

    ! sorting loop
    do
        ! if we didnt swap, we exit
        if(swap .eqv. .false.) exit

        ! set swap to false
        swap = .false.
        
        ! iterate the array and swap if required
        do i = 1, msize
            if(array(i) > array(i + 1)) then
                temp = array(i)
                array(i) = array(i + 1)
                array(i + 1) = temp
                ! if we swapped, set the variable to true
                swap = .true.
            end if
        end do
    end do
end subroutine bubble_sort

! Selection Sort implementation
! msize     : integer, input, size of the array
! array     : integer(msize), input / output
!!
subroutine selection_sort(msize, array)
    implicit none
    ! variables
    integer, intent(in) :: msize
    integer, intent(inout) :: array(msize)
    integer :: lowest_index, i, j, temp

    ! iterate the array
    do i = 1, msize
        ! assume the first element is the lowest element
        lowest_index = i
        ! iterate the unsorted items
        do j = i + 1, msize
            if (array(j) < array(lowest_index)) then
                lowest_index = j
            end if
        end do
        ! swap values
        temp = array(i)
        array(i) = array(lowest_index)
        array(lowest_index) = temp
    end do
end subroutine selection_sort

! Insertion Sort implementation
! msize     : integer, input, size of the array
! array     : integer(msize), input / output
!!
subroutine insertion_sort(msize, array)
    implicit none
    ! variables
    integer, intent(in) :: msize
    integer, intent(inout) :: array(msize)
    integer :: prev_index, i, insert

    do i = 1, msize
        ! current item to insert
        insert = array(i)
        ! previous index
        prev_index = i - 1
        ! if the item is larger, move everything forward
        do while ((prev_index > 0) .and. (array(prev_index) > insert))
            array(prev_index + 1) = array(prev_index)
            prev_index = prev_index - 1
        end do
        ! insert the item
        array(prev_index + 1) = insert
    end do
end subroutine insertion_sort

! Quick Sort algorithm
! implementation inspired by https://gist.github.com/t-nissie/479f0f16966925fa29ea
! (and greatly improved because of that - thanks!)
! msize     : integer, input, size of the array
! array     : integer(msize), input / output
! first     : integer, input - first element of the array (1 for the first call)
! last      : integer, input - last element of the array (size(array) for the first call)
!!
recursive subroutine quick_sort(msize, array, first, last)
    implicit none
    ! variables
    integer, intent(inout) :: array(msize)
    integer, intent(in) :: first, last, msize
    integer :: pivot, temp, i, j

    ! save first, last values
    i = first
    j = last

    ! calculate the pivot
    pivot = array( (i + j) / 2)

    do
        do while (array(i) < pivot)
            i = i + 1
        end do

        do while (array(j) > pivot)
            j = j - 1
        end do

        if(i >= j) exit

        ! swap the items
        temp = array(i)
        array(i) = array(j)
        array(j) = temp

        ! increment / decrement
        i = i + 1
        j = j - 1
    end do

    ! recursive calls
    if(first < i - 1) call quick_sort(msize, array, first, i - 1)
    if(j + 1 < last) call quick_sort(msize, array, j + 1, last)

end subroutine quick_sort
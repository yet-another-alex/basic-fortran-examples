!!
! basic example for taking user input and displaying some output with those variables
!!
program read_input
    implicit none

    ! variables to get input for
    integer :: a
    integer :: b

    ! variables for output
    integer :: addition
    integer :: subtraction
    integer :: multiplication
    real :: division
    real :: exponent

    ! get input for a
    print *, 'Please enter a number: '
    read(*, *) a

    ! get input for b
    print *, 'Please enter another number: '
    read(*, *) b

    ! calculate the output variables
    addition = a + b
    subtraction = a - b
    multiplication = a * b
    division = a / b
    exponent = a ** b

    ! output the results
    print *, 'Addition: ', addition
    print *, 'Subtraction: ', subtraction
    print *, 'Multiplication: ', multiplication
    print *, 'Division: ', division
    print *, 'exponent: ', exponent

end program read_input
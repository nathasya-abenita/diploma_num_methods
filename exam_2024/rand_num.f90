PROGRAM apply_monte_carlo
    USE monte_carlo
    IMPLICIT NONE
    INTEGER :: n, i ! Number of random numbers, loop index
    INTEGER :: n_c ! Accepted value counter
    REAL :: x, y, area, vol ! Random number to be generated
    REAL, DIMENSION(:), ALLOCATABLE :: gen ! Random numbers to be generated
    
    ! Initialize number of experiment and counter
    n = INT(1E5)
    n_c = 0

    ! (a). Applying rejection method to generate from PDF f(x)

    ! Prepare output array
    ALLOCATE(gen(n))
    
    ! Argument: (n, x_min, x_max, f_max, output_array)
    CALL generate_by_rejection (n, 0.0, 10.0, 1.0, gen)

    ! Print out
    OPEN (UNIT=11, FILE='./exam_2024/rejection.txt', STATUS='replace', ACTION='write')
        WRITE(11, *) gen
    CLOSE(11)

    ! (b). Estimate integral with crude Monte-Carlo
    CALL integrate_by_monte_carlo (n, 0.0, 2.0, area)

    ! Print output
    PRINT*, 'Integral of x exp(-x**2) between 0 and 2 is', area

END PROGRAM apply_monte_carlo
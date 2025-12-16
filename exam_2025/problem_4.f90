
MODULE matrix_operator
    USE module_sorter
    IMPLICIT NONE
CONTAINS
    REAL FUNCTION average(arr) RESULT(avg)
        REAL, DIMENSION (:), INTENT(IN) :: arr ! one-dimensional input
        INTEGER :: n ! Size variable
        INTEGER :: i ! indexing variable

        ! Take size of the array
        n = SIZE(arr) 
        
        ! Apply average formula
        avg = SUM(arr) / n
    END FUNCTION average

    REAL FUNCTION standard_deviation(arr) RESULT(std)
        REAL, DIMENSION (:), INTENT(IN) :: arr
        INTEGER :: n 

        ! Take size of the array
        n = SIZE(arr)

        ! Apply standard deviation formula (using unbiased estimator)
        std = ( SUM( (arr - average(arr)) ** 2 ) / (n-1) ) ** 0.5
    END FUNCTION standard_deviation
END MODULE matrix_operator

MODULE monte_carlo
    IMPLICIT NONE
CONTAINS
    ! Call random number from dist. of U[a, b]
    SUBROUTINE gen_uniform (x, a, b)
        REAL, INTENT(INOUT) :: x
        REAL, INTENT(IN) :: a, b

        ! Generate a number from U[0, 1]
        CALL RANDOM_NUMBER(x)
        ! Shift to distribution of U[a, b]
        x = a + (b - a) * x
    END SUBROUTINE gen_uniform

    ! FUNCTIONS

    REAL FUNCTION func_f (x) RESULT(res) ! For rejection method
        REAL, INTENT(IN) :: x

        ! Define pi
        REAL :: pi
        pi = 4 * ATAN(1.0)

        ! Build step-wise function
        IF ((x >= -1) .OR. (x <= 1)) THEN
            res = 2 * SQRT(1 - x**2) / pi
        ELSE
            res = 0
        END IF
    END FUNCTION func_f

    REAL FUNCTION func_g (x) RESULT(res) ! For crude Monte-Carlo method
        REAL, INTENT(IN) :: x

        res = x**2 * func_f(x)
    END FUNCTION func_g

    ! GENERATE RANDOM NUMBERS AND MONTE-CARLO METHODS

    SUBROUTINE generate_by_rejection (n, x_min, x_max, f_max, gen)
        INTEGER, INTENT(IN) :: n
        REAL, INTENT(IN) :: x_min, x_max, f_max
        REAL, DIMENSION(:), INTENT(OUT) :: gen ! Size of n
        REAL :: x ! x ~ U[x_min, x_max]
        REAL :: prob, r ! Acceptance probability and r ~ U[0, 1]
        INTEGER :: i = 1 ! Number of generated number

        DO
            ! Generate random number from U[x_min, x_max]
            CALL gen_uniform(x, x_min, x_max)

            ! Calculate acceptance of probability
            prob = func_f(x) / f_max

            ! Call random number from U[0, 1]
            CALL RANDOM_NUMBER(r)

            ! Check acceptance
            IF (r <= prob) THEN
                gen(i) = x
                i = i + 1
            END IF

            ! Stop loop if n numbers have been generated
            IF (i == (n + 1)) EXIT
        END DO
    END SUBROUTINE generate_by_rejection

    SUBROUTINE integrate_by_monte_carlo (n, a, b, area)
        INTEGER, INTENT(IN) :: n
        REAL, INTENT(IN) :: a, b
        REAL, INTENT(OUT) :: area
        REAL :: x ! Generated random number
        INTEGER :: i ! Looping index

        ! Initialize area / integral output
        area = 0

        ! Perform trials
        DO i = 1, n
            ! Call random number
            CALL gen_uniform(x, a, b)

            ! Update area
            area = area + func_g(x)
        END DO

        ! Finalize
        area = area * (b - a) / n
    END SUBROUTINE integrate_by_monte_carlo
END MODULE monte_carlo

PROGRAM apply_monte_carlo_int
    USE matrix_operator
    USE monte_carlo
    IMPLICIT NONE
    INTEGER :: n, i ! Number of random numbers, loop index
    REAL :: x, y ! Random number to be generated
    REAL, DIMENSION(:), ALLOCATABLE :: gen ! Random numbers to be generated
    REAL :: area ! Integration result
    REAL :: f_max ! Maximum value of the function

    ! Define pi
    REAL :: pi
    pi = 4 * ATAN(1.0)
    
    ! Initialize number of experiment and counter
    n = INT(1E6)

    ! (a). Applying rejection method to generate from PDF f(x)

    ! Prepare output array
    ALLOCATE(gen(n))

    ! Define f_max
    f_max = 2 / pi
    
    ! Argument: (n, x_min, x_max, f_max, output_array)
    CALL generate_by_rejection (n, -1.0, 1.0, f_max, gen)

    ! Print out its mean and variance
    PRINT*, 'Generating random numbers with rejection method...'
    PRINT('(2A10)'), 'MEAN', 'VAR'
    PRINT('(2F10.5)'), average(gen), standard_deviation(gen) ** 2

    ! Print out generated number to check its histogram
    OPEN (UNIT=11, FILE='rejection.txt', STATUS='replace', ACTION='write')
        WRITE(11, *) gen
    CLOSE(11)

    ! (b). Estimate integral with crude Monte-Carlo
    CALL integrate_by_monte_carlo (n, -1.0, 1.0, area)

    ! Print output
    PRINT*, 'Integral of x^2 f(x) between -1 and 1 is', area
    PRINT*, 'The value of the integral should be the unbiased variance of the samples!'
END PROGRAM apply_monte_carlo_int
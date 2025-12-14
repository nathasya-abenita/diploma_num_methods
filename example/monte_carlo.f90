MODULE monte_carlo
    IMPLICIT NONE
CONTAINS

    ! FUNCTIONS

    REAL FUNCTION func_f (x) RESULT(res)
        REAL, INTENT(IN) :: x

        res = 3.0 * x ** 2.0
    END FUNCTION func_f

    REAL FUNCTION func_g (x) RESULT(res)
        REAL, INTENT(IN) :: x

        res = EXP(-1.0 * x ** 2.0)
    END FUNCTION func_g

    REAL FUNCTION func_g2d (x, y) RESULT(res)
        REAL, INTENT(IN) :: x, y

        IF ( ( x** 2 + y**2 ) <= 1 ) THEN
            res = 1
        ELSE
            res = 0
        END IF
    END FUNCTION func_g2d

    ! GENERATE RANDOM NUMBERS AND MONTE-CARLO METHODS

    SUBROUTINE gen_uniform (x, a, b)
        REAL, INTENT(INOUT) :: x
        REAL, INTENT(IN) :: a, b

        ! Generate a number from U[0, 1]
        CALL RANDOM_NUMBER(x)
        ! Shift to distribution of U[a, b]
        x = a + (b - a) * x
    END SUBROUTINE gen_uniform

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

    SUBROUTINE integrate_by_hit_miss (n, a, b, f_max, area)
        INTEGER, INTENT(IN) :: n
        REAL, INTENT(IN) :: a, b, f_max
        REAL, INTENT(OUT) :: area
        REAL :: x, y ! Generated random number
        INTEGER :: i, count

        ! Initialize counter
        count = 0

        ! Perform trials
        DO i = 1, n
            ! Generate numbers
            CALL gen_uniform(x, a, b)
            CALL gen_uniform(y, 0.0, f_max)

            ! Acceptance check
            IF (y < func_f(x)) THEN
                count = count + 1
            END IF
        END DO

        ! Finalize area
        area = count / REAL(n) * (b - a) * f_max
    END SUBROUTINE integrate_by_hit_miss

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

    SUBROUTINE integrate_by_monte_carlo_2d (n, x1, x2, y1, y2, vol)
        INTEGER, INTENT(IN) :: n
        REAL, INTENT(IN) :: x1, x2, y1, y2
        REAL, INTENT(OUT) :: vol
        REAL :: x, y ! Generated random number
        INTEGER :: i ! Looping index

        ! Initialize area / integral output
        vol = 0

        ! Perform trials
        DO i = 1, n

            ! Call random number
            CALL gen_uniform(x, x1, x2)
            CALL gen_uniform(y, y1, y2)

            ! Update area
            vol = vol + func_g2d(x, y)
        END DO

        ! Finalize
        vol = vol * (x2 - x1) * (y2 - y1) / n
    END SUBROUTINE integrate_by_monte_carlo_2d
END MODULE monte_carlo

PROGRAM apply_monte_carlo
    USE monte_carlo
    IMPLICIT NONE
    INTEGER :: n, i ! Number of random numbers, loop index
    INTEGER :: n_c ! Accepted value counter
    REAL :: x, y, area, vol ! Random number to be generated
    REAL, DIMENSION(:), ALLOCATABLE :: gen ! Random numbers to be generated
    
    ! Initialize number of experiment and counter
    n = INT(1E4)
    n_c = 0
    
    ! 1. Approximate pi with Hit-or-Miss method

    DO i = 1, n
        ! Generate to U[-1, 1]
        CALL gen_uniform(x, -1.0, 1.0)
        CALL gen_uniform(y, -1.0, 1.0)

        ! Check acceptance
        IF ((x ** 2 + y ** 2) <= 1) THEN
            n_c = n_c + 1
        END IF
    END DO 

    ! Print output
    PRINT*, 'Approximated value of pi:', 4.0 * n_c / n

    ! 2. Applying rejection method to generate from PDF f(x)

    ! Prepare output array
    ALLOCATE(gen(n))
    
    ! Argument: (n, f_min, f_max, output_array)
    CALL generate_by_rejection (n, 0.0, 1.0, 3.0, gen)

    ! Print out
    OPEN (UNIT=11, FILE='./output/rejection.txt', STATUS='replace', ACTION='write')
        WRITE(11, *) gen
    CLOSE(11)

    ! 3. Estimate integral with Hit-and-Miss

    ! Argument : (n, a, b, f_max, area)
    CALL integrate_by_hit_miss (n, 0.0, 3.0, 27.0, area)

    ! Print output
    PRINT*, 'Integral of 3x^2 between 0 and 3 is', area

    ! 4. Estimate integral with crude Monte-Carlo
    CALL integrate_by_monte_carlo (n, 0.0, 1.0, area)

    ! Print output
    PRINT*, 'Integral of exp(-x**2) between 0 and 1 is', area

    ! 5. Estimate pi with crude Monte-Carlo method
    CALL integrate_by_monte_carlo_2d (n, -1.0, 1.0, -1.0, 1.0, vol)

    ! Print output
    PRINT*, 'Integral of F(x, y) between [0, 1] x [0, 1] is', vol
END PROGRAM apply_monte_carlo
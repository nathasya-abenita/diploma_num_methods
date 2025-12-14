
!===========================================================
! Trying to apply middle square and linear congruential 
! methods to generate random numbers.
!===========================================================

MODULE random_generator
    IMPLICIT NONE
CONTAINS
    SUBROUTINE middle_square (arr, seed_initial)
        REAL, DIMENSION(:), INTENT(INOUT) :: arr
        INTEGER, INTENT(IN) :: seed_initial
        REAL :: seed, d
        INTEGER :: i ! Looping index, digit

        seed = 1.0 * seed_initial
        
        ! Take the digit of the number
        d = LOG10(ABS(seed)) + 1

        ! Generate random seeds
        DO i = 1, SIZE(arr)
            seed = seed ** 2
            seed = MOD(seed / (10 ** (0.5 * d)), 10 ** d)
            arr(i) = seed
        END DO

        ! Normalize to [0, 1] interval
        arr = arr / 10 ** d
    END SUBROUTINE middle_square

    SUBROUTINE linear_congruential_method (arr, seed)
        REAL, DIMENSION(:), INTENT(INOUT) :: arr
        INTEGER, INTENT(IN) :: seed
        INTEGER :: i ! Looping index, digit
        REAL :: a, c, m ! LCM parameters

        ! Define parameters
        a = 5
        m = 16
        c = 3

        ! Generate random numbers
        DO i = 1, SIZE(arr)
            IF (i == 1) THEN
                arr(i) = MOD((a * seed + c), m)
            ELSE
                arr(i) = MOD((a * arr(i - 1) + c), m)
            END IF
        END DO

        ! Normalize to [0, 1] interval
        arr = arr / m
    END SUBROUTINE linear_congruential_method

    SUBROUTINE resize_shape (arr, a, b)
        REAL, DIMENSION(:), INTENT(INOUT) :: arr
        REAL, INTENT(IN) :: a, b

        arr = a + (b - a) * arr
    END SUBROUTINE resize_shape

END MODULE random_generator

PROGRAM generate_random_numbers
    USE random_generator
    IMPLICIT NONE
    INTEGER, PARAMETER :: n = 10 ! Number of generated random numbers
    REAL :: a, b ! Bounds of the uniform
    INTEGER :: seed = 675248
    REAL, DIMENSION(n) :: generated_numbers

    ! Define the bounds
    a = 0
    b = 10

    ! Generate the numbers using middle square method
    CALL middle_square(generated_numbers, seed)
    CALL resize_shape(generated_numbers, a, b)
    PRINT*, generated_numbers

    ! Generate the numbers using linear congruential method
    CALL linear_congruential_method(generated_numbers, seed)
    CALL resize_shape(generated_numbers, a, b)
    PRINT*, generated_numbers
END PROGRAM

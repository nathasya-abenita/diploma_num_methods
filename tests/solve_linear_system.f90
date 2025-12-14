PROGRAM apply_cramer_rule
    USE linear_system
    IMPLICIT NONE
    INTEGER, PARAMETER :: n_size = 3 ! Matrix size (can be 2 or 3)
    REAL, DIMENSION(n_size, n_size) :: a ! Coefficient matrix
    REAL, DIMENSION(n_size) :: x, b ! Solution vectors
    INTEGER :: i ! Looping index
    INTEGER :: unit_num = 11 ! Unit number for accessing files

    ! Open input data file
    OPEN(UNIT=unit_num, FILE='./data_sample/matrix.dat', STATUS='old', ACTION='read')

    ! Read coefficient matrix elements, saving it row by row
    DO i = 1, n_size
        READ(unit_num, *) a(i, :)
    END DO

    ! Read empty line
    READ(unit_num, *)

    ! Read line for known vector b
    READ(unit_num, *) b(:)
    CLOSE(unit_num)

    ! Find solution by calling subroutine
    CALL cramer_rule(a, b, x)

    ! Print solution
    PRINT*, 'With cramer rule, we obtain (x1, x2, x3) =', x
END PROGRAM apply_cramer_rule
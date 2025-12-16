
!===========================================================
!   Module: Matrix Operator
!   Purpose: Basic functions relating to array and matrices
!===========================================================

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

        ! Apply standard deviation formula
        std = ( SUM( (arr - average(arr)) ** 2 ) / n ) ** 0.5
    END FUNCTION standard_deviation

    ! Matrices operators

    REAL FUNCTION determinant(mat) RESULT(det)
        REAL, DIMENSION (:, :), INTENT(IN) :: mat
        INTEGER :: n, i ! Matrix size and indexing variable
        REAL :: m11, m12, m13, m21, m22, m23, m31, m32, m33 ! Matrix' element

        ! Check matrix (BONUS)
        CALL check_matrix(mat)

        ! Take size of the array
        n = SIZE(mat, 1)

        ! Check case by size
        IF (n == 2) THEN
            det = mat(1, 1) * mat(2, 2) - mat(1, 2) * mat(2, 1)
        ELSE ! n == 3, apply Sarrus' rule, column is the first dimension
        ! I think there must be a better way to write this easier
            m11 = mat(1, 1)
            m21 = mat(2, 1)
            m31 = mat(3, 1)
            m12 = mat(1, 2)
            m22 = mat(2, 2)
            m32 = mat(3, 2)
            m13 = mat(1, 3)
            m23 = mat(2, 3)
            m33 = mat(3, 3)
            det = m11*m22*m33 + m21*m32*m13 + m31*m12*m23 - m31*m22*m13 - m11*m32*m23 - m21*m12*m33
        END IF
    END FUNCTION determinant

    SUBROUTINE check_matrix (mat)
        REAL, DIMENSION(:, :), INTENT(IN) :: mat
        INTEGER :: n ! Matrix first dimension size

        ! BONUS: Ensure matrix is size is 2 or 3
        n = SIZE(mat, 1)
        IF ((n /= 2) .AND. (n /= 3)) STOP 'Inputted matrix has to be 2 x 2 or 3 x 3!'

        ! BONUS: Ensure matrix is square
        IF (n**2 /= SIZE(mat)) STOP 'Inputted matrix has to be square!'    
    END SUBROUTINE check_matrix
END MODULE matrix_operator

!===========================================================
!   Module: Linear system
!   Purpose: Solve linear system by using Cramer rule
!===========================================================

MODULE linear_system
    USE matrix_operator
    IMPLICIT NONE
CONTAINS
    SUBROUTINE cramer_rule (a_in, b, x)
        REAL, DIMENSION(:, :), INTENT(IN) :: a_in
        REAL, DIMENSION(:, :), ALLOCATABLE :: a ! Coefficient matrix that its i-column will be updated according to solution x_i
        REAL, DIMENSION(:), INTENT(IN) :: b
        REAL, DIMENSION(:), INTENT(OUT) :: x
        INTEGER :: i, n ! Looping index and matrix size
        REAL :: det_a

        ! Compute matrix size and coefficient matrix determinant
        n = SIZE(b)
        det_a = determinant(a_in)

        ! Stop program if determinant is zero
        IF (det_a == 0) STOP "The determinant of the coefficient matrix is zero!"

        ! Allocate matrix size for the coefficient matrix
        ALLOCATE(a(n, n))

        ! Loop over column index
        DO i = 1, n
            ! Initialize coefficient matrix
            a = a_in

            ! Prepare updated coefficient matrix
            a(:, i) = b

            ! Compute the solution
            x(i) = determinant(a) / det_a
        END DO
    END SUBROUTINE cramer_rule
END MODULE linear_system

!===========================================================
!   Module: Linear fit
!   Purpose: Perform linear fit for one dimensional function
!   with two parameters (2D-fit)
!===========================================================

MODULE linear_fit
    USE matrix_operator
    USE linear_system
    IMPLICIT NONE
CONTAINS

    SUBROUTINE linear_fit_2d (x, y, b1, b2)
        REAL, DIMENSION(:), INTENT(IN) :: x, y
        REAL, INTENT(OUT) :: b1, b2
        INTEGER :: n
        REAL, DIMENSION(2, 2) :: a ! Coefficient matrix
        REAL, DIMENSION(2) :: x_sol, b ! Solution vectors
        
        ! Data size
        n = SIZE(x)

        ! Prepare coefficient matrix to the linear system
        a(1, 1) = n
        a(1, 2) = SUM(x)
        a(2, 1) = a(1, 2)
        a(2, 2) = SUM(x**2)

        ! Prepare the right hand side of the linear system
        b(1) = SUM(y)
        b(2) = SUM(x * y)
        
        ! Solve the linear system to find fit parameters
        CALL cramer_rule (a, b, x_sol)
        b1 = x_sol(1)
        b2 = x_sol(2)
    END SUBROUTINE linear_fit_2d

    SUBROUTINE quality_fit (x, y, b1, b2, std_1, std_2, r_sq)
        REAL, DIMENSION(:), INTENT(IN) :: x, y
        REAL, INTENT(IN) :: b1, b2
        REAL, INTENT(OUT) :: std_1, std_2, r_sq
        REAL :: var, x_bar, y_bar
        INTEGER :: n
        
        ! Data size
        n = SIZE(x)

        ! Compute R^2
        y_bar = (SUM(y) / n)
        r_sq = 1 - SUM((y - (b1 + b2 * x)) ** 2) &
               / SUM((y - y_bar)**2)

        ! Compute standard errors
        var = SUM((y - (b1 + b2 * x))**2 / (n-2))
        x_bar = (SUM(x) / n)
        std_1 = SQRT(var) * SQRT(1/n + (x_bar**2 / SUM((x - x_bar)**2)))
        std_2 = SQRT(var) / SQRT( SUM( (x - x_bar)**2 ) )
    END SUBROUTINE quality_fit
    
END MODULE linear_fit

PROGRAM perform_linear_fit
    USE linear_fit
    IMPLICIT NONE
    INTEGER, PARAMETER :: data_len = 228 ! Length of data
    INTEGER, PARAMETER :: n_size = 2 ! Matrix size
    REAL, DIMENSION(data_len) :: x, y ! To store data where y is response of x
    INTEGER :: i ! Looping index
    INTEGER :: unit_num = 11 ! Unit number for accessing files
    REAL :: b1, b2, r_sq, std_1, std_2 ! Fit parameters, the coef. of determination, and std. err.
    CHARACTER(LEN=11) :: str_format = '(A20 F20.5)'

    ! Final fit parameters by reverse-transformation
    REAL :: out_a, out_tau, t_half

    ! Open input data file
    OPEN(UNIT=unit_num, FILE='C14.txt', STATUS='old', ACTION='read')

    ! Read header
    READ(unit_num, *)

    ! Read data
    DO i = 1, data_len
        READ(unit_num, *) x(i), y(i)
    END DO
    PRINT*, 'Checking last read row:', x(data_len), y(data_len)
    CLOSE(unit_num) ! Close data reading

    ! Transform y with LOG function
    y = LOG(y)

    ! Perform fit with linear regression
    CALL linear_fit_2d (x, y, b1, b2)
    CALL quality_fit (x, y, b1, b2, std_1, std_2, r_sq)

    ! Compute final parameters
    out_a = EXP(b1)
    out_tau = -1.0 / b2
    t_half = LOG(2.0) * out_tau

    ! Write output
    OPEN(UNIT=unit_num, FILE='fit.txt', STATUS='replace', ACTION='write')

    ! Fit results, coef. of determination (R^2), and standard errors
    WRITE(unit_num, str_format) 'beta_1', b1
    WRITE(unit_num, str_format) 'beta_2', b2
    WRITE(unit_num, str_format) 'R^2', r_sq
    WRITE(unit_num, str_format) 'std for beta_1', std_1
    WRITE(unit_num, str_format) 'std for beta_2', std_2

    ! Final fit parameters (A and tau)
    WRITE(unit_num, str_format) 'A', out_a
    WRITE(unit_num, str_format) '\tau', out_tau

    ! Half life time
    WRITE(unit_num, str_format) 't_{1/2} (yr)', t_half
    CLOSE(unit_num)

    ! Give output info
    PRINT*, 'All outputs are printed in file fit.txt!'
END PROGRAM perform_linear_fit
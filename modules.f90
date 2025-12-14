!===========================================================
!   Module: Matrix Operator
!   Purpose: Basic functions relating to array and matrices
!===========================================================

MODULE matrix_operator
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

    REAL FUNCTION maximum(arr) RESULT(max)
        REAL, DIMENSION(:), INTENT(IN) :: arr
        INTEGER :: i ! Counter variable

        max = arr(1) ! First guess

        ! Iterate over each element to keep the biggest value
        DO i = 2, SIZE(arr)
            IF (arr(i) > max) max = arr(i)
        END DO
    END FUNCTION maximum

    ! Matrices operators

    REAL FUNCTION trace(mat) RESULT(tr)
        REAL, DIMENSION (:, :), INTENT(IN) :: mat
        INTEGER :: n, i ! Matrix size and indexing variable
        
        ! Check matrix (BONUS)
        CALL check_matrix(mat)

        ! Take size of the array
        n = SIZE(mat, 1)

        ! Compute trace
        tr = 0
        DO i = 1, n
            tr = tr + mat(i, i)
        END DO
    END FUNCTION trace

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
!   Module: Read file
!   Purpose: Basic file reading
!===========================================================

MODULE read_file
    IMPLICIT NONE
CONTAINS
    SUBROUTINE read_flexible (file_name, mat, n_col, n_row)
        CHARACTER(LEN=1) :: first_char
        CHARACTER(LEN=80) :: line
        INTEGER :: unit_num = 11
        INTEGER :: i, err, n_trim ! Counter variables
        INTEGER :: row_start = 0 ! Starting line
        
        CHARACTER(LEN=*), INTENT(IN) :: file_name
        REAL, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: mat
        INTEGER, INTENT(OUT) :: n_col, n_row

        ! Initialize number of rows and columns
        n_col = 0
        n_row = 0

        ! Open file
        OPEN(UNIT=unit_num, FILE=file_name, STATUS='old', ACTION='read')

        ! Reading number of comment lines in header
        DO
            READ(unit_num, *, iostat=err) first_char

            IF (first_char == '#') THEN
                row_start = row_start + 1
            ELSE
                EXIT
            END IF
        END DO
        PRINT*, 'Start line (without header):', row_start

        ! Reading number of rows, ignoring hash symbol, and saving information
        REWIND(unit_num)
        DO
            READ(unit_num, *, iostat=err) first_char
            IF (err /= 0) EXIT
            IF (first_char /= '#') n_row = n_row + 1
        END DO
        PRINT*, 'Number of rows:', n_row
        
        ! Reading number of columns
        REWIND(unit_num)
        READ(unit_num, '(A)') line
        n_trim = LEN_TRIM(line) ! Remove trailing whitespace
        DO i = 1, n_trim - 1
            IF (line(i: i+1) == '  ') n_col = n_col + 1 ! Find number of seperator
        END DO
        PRINT*, 'Number of col.:', n_col
        
        ! Define data variable size
        ALLOCATE(mat(n_row, n_col))

        ! Prepare to read file by skipping header lines
        REWIND(unit_num)
        IF (row_start > 0) THEN
            DO i = 1, row_start
                READ(unit_num, *)
            END DO
        END IF

        ! Read file
        DO i = 1, n_row
            READ(unit_num, *) mat(i, :)
        END DO

        ! Close file    
        CLOSE(unit_num)
    END SUBROUTINE
END MODULE read_file

!===========================================================
!   Module: Root Finder
!   Purpose: Different methods to perform root finding,
!   completed with related printing subroutine to test it.
!   Methods: Regula Falsi, Bisection, Newton, Secant
!===========================================================

MODULE root_finder
    IMPLICIT NONE
    INTEGER, PARAMETER :: rpm = 8 ! Real parameter
CONTAINS

    ! Function in which the root will be found
    REAL FUNCTION func_f (x) RESULT(f)
        REAL(rpm), INTENT(IN) :: x
        !f = x**5 + 3*x**4 - 2*x**2 - LOG(x**2) - 2
        f = COS(x) - LOG(x) + 1
    END FUNCTION func_f

    REAL FUNCTION func_f_prime (x) RESULT(f)
        REAL(rpm), INTENT(IN) :: x
        f = -1.0 * SIN(x) - (1.0 / x) 
    END FUNCTION func_f_prime

    SUBROUTINE bisection (a, b, eps)
        REAL(rpm), INTENT(IN) :: a, b, eps
        REAL(rpm) :: a0, b0
        REAL(rpm) :: c ! Root
        INTEGER :: i = 0 ! Iteration index
        
        ! Initialize values
        a0 = a
        b0 = b

        DO
            ! Update root guess
            c = (a0 + b0) / 2
            
            ! Swap bound
            IF ( (func_f(a0) * func_f(c)) <= 0 ) THEN
                b0 = c
            ELSE
                a0 = c
            END IF

            ! Update iteration number
            i = i + 1

            ! Exit iteration according when error is small enough
            IF ((ABS(a0 - b0)) < eps) EXIT
        END DO 

        ! Print output
        CALL print_output(c, i)
    END SUBROUTINE bisection

    SUBROUTINE regula_falsi (a, b, eps)
        REAL(rpm), INTENT(IN) :: a, b, eps
        REAL(rpm) :: a0, b0
        REAL(rpm) :: c ! Root
        INTEGER :: i = 0 ! Iteration index
        REAL(rpm) :: error

        ! Initialize values
        a0 = a
        b0 = b

        DO
            ! Update root guess
            c = (func_f(b0) * a0 - func_f(a0) * b0) / (func_f(b0) - func_f(a0))
            
            ! Swap bound
            IF ( (func_f(a0) * func_f(c)) <= 0 ) THEN
                b0 = c
            ELSE
                a0 = c
            END IF

            ! Update iteration number
            i = i + 1

            ! Exit iteration according when error is small enough
            IF ((ABS(a0 - b0)) < eps) EXIT
        END DO

        ! Print output
        CALL print_output(c, i)
    END SUBROUTINE regula_falsi

    SUBROUTINE newton (x, eps)
        REAL(rpm), INTENT(IN) :: x, eps ! Initial guess and error criteria
        REAL(rpm) :: c, c_old ! Root (current step and previous step)
        REAL(rpm) :: f, f_prime ! Function values
        INTEGER :: i = 0 ! Iteration index
        
        c_old = x ! Initialize roots with initial guess        

        DO
            ! Update iteration number
            i = i + 1 
            
            ! Compute function values
            f = func_f(c_old)
            f_prime = func_f_prime(c_old)

            ! Stop algorithm if the derivative is zero
            IF (ABS(f_prime) < TINY(x)) STOP 'Algorithm is failed!'

            ! Update root
            c = c_old - f / f_prime

            ! Exit iteration when error is small enough
            IF ((ABS(c - c_old)) < eps) EXIT

            ! Update old root
            c_old = c
        END DO

        ! Print output
        CALL print_output(c, i)
    END SUBROUTINE

    SUBROUTINE secant (x0, x1, eps)
        REAL(rpm), INTENT(IN) :: x0, x1, eps ! Initial guesses and error criteria
        REAL(rpm) :: c0, c1, c2 ! Root (current step and previous step)
        REAL(rpm) :: f0, f1 ! Function value
        INTEGER :: i = 0 ! Iteration index
        
        ! Initialize roots with initial guess
        c0 = x0
        c1 = x1

        DO
            ! Update iteration number
            i = i + 1 
            
            ! Compute function values
            f0 = func_f(c0)
            f1 = func_f(c1)

            ! Stop algorithm if the derivative is zero
            IF (ABS(f1 - f0) < TINY(x0)) STOP 'Algorithm is failed!'

            ! Update root
            c2 = c1 - (f1 * (c1 - c0)) / (f1 - f0)

            ! Exit iteration when error is small enough
            IF ((ABS(c2- c1)) < eps) EXIT

            ! Update old root
            c0 = c1
            c1 = c2            
        END DO

        ! Print output
        CALL print_output(c2, i)
    END SUBROUTINE

    SUBROUTINE print_output(c, i)
        REAL(rpm), INTENT(IN) :: c
        INTEGER, INTENT(IN) :: i

        PRINT*, 'Number of iteration:', i
        PRINT*, 'Root solution:', c
    END SUBROUTINE print_output

    SUBROUTINE perform_all_methods(a, b, eps, single_guess)
        REAL(rpm), INTENT(IN) :: a, b, eps, single_guess

        ! Print error criteria
        PRINT*, 'Root finding problem will be performed with error of', eps

        ! Call subroutine to perform the root finding algorithms
        PRINT*, 'USING REGULA FALSI METHOD'
        CALL regula_falsi(a, b, eps)

        PRINT*, 'USING BISECTION METHOD'
        CALL bisection(a, b, eps)

        PRINT*, 'USING NEWTON METHOD'
        CALL newton(single_guess, eps)

        PRINT*, 'USING SECANT METHOD'
        CALL SECANT(a, b, eps)
    END SUBROUTINE
END MODULE

!===========================================================
!   Module: Linear interpolation
!   Purpose: Perform linear interpoolation with two options
! of extrapolation: linear or constant
!===========================================================

MODULE linear_interpolation
    IMPLICIT NONE
CONTAINS
    ! This is a function to perform interpolation for a single predictor
    ! Assumption: the predictor is in range with the data
    REAL FUNCTION interpolate (vec_t, vec_m, t_new) RESULT(res)
        REAL, DIMENSION(:), INTENT(IN) :: vec_t, vec_m
        REAL, INTENT(IN) :: t_new
        INTEGER :: n, i
        
        ! Data size
        n = SIZE(vec_t)

        ! Find index such that t_new is in between vec_t(i) and vec_t(i+1)
        i = 1
        DO
            IF ((t_new >= vec_t(i)) .AND. (t_new <= vec_t(i+1))) EXIT
            i = i + 1
        END DO 

        ! Perform interpolation
        res = vec_m(i) + (vec_m(i+1) - vec_m(i)) / (vec_t(i+1) - vec_t(i)) * (t_new - vec_t(i))
    END FUNCTION interpolate

    ! This is a function to perform extrapolation (constant method) for a single predictor
    ! Assumption: the predictor is out of range with the data or the same as the bounds
    REAL FUNCTION extrapolate_constant (vec_t, vec_m, t_new) RESULT(res)
        REAL, DIMENSION(:), INTENT(IN) :: vec_t, vec_m
        REAL, INTENT(IN) :: t_new
        INTEGER :: n

        ! Data size
        n = SIZE(vec_t)

        ! Perform extrapolation by checking t_new location
        IF (t_new >= vec_t(n)) THEN
            res = vec_m(n)
        ELSE
            res = vec_m(1)
        END IF
    END FUNCTION extrapolate_constant

    ! This is a function to perform extrapolation (linear method) for a single predictor
    ! Assumption: the predictor is out of range with the data or the same as the bounds
    REAL FUNCTION extrapolate_linear (vec_t, vec_m, t_new) RESULT(res)
        REAL, DIMENSION(:), INTENT(IN) :: vec_t, vec_m
        REAL, INTENT(IN) :: t_new
        INTEGER :: n

        ! Data sizet
        n = SIZE(vec_t)

        ! Inputs: first two starting time, last two
        IF (t_new >= vec_t(n)) THEN
            res = vec_m(n) + (vec_m(n) - vec_m(n-1)) / (vec_t(n) - vec_t(n-1)) * (t_new - vec_t(n))
        ELSE
            res = vec_m(1) + (vec_m(2) - vec_m(1)) / (vec_t(2) - vec_t(1)) * (t_new - vec_t(1))
        END IF
    END FUNCTION extrapolate_linear

    SUBROUTINE linear_interpolation_constant_extrapolation (vec_t, vec_m, vec_t_new, vec_m_new)
        REAL, DIMENSION(:), INTENT(IN) :: vec_t, vec_m, vec_t_new
        REAL, DIMENSION(:), INTENT(OUT) :: vec_m_new
        INTEGER :: n, i

        ! Data size
        n = SIZE(vec_t)

        ! Perform extrapolation
        DO i = 1, n
            IF (vec_t_new(i) <= vec_t(1) .OR. vec_t_new(i) >= vec_t(n)) THEN ! Case 1: Extrapolation for values outside of data range
                vec_m_new(i) = extrapolate_constant(vec_t, vec_m, vec_t_new(i))
            ELSE ! Case 2: Perform interpolation
                vec_m_new(i) = interpolate(vec_t, vec_m, vec_t_new(i))
            END IF
        END DO
    END SUBROUTINE linear_interpolation_constant_extrapolation

    SUBROUTINE linear_interpolation_linear_extrapolation (vec_t, vec_m, vec_t_new, vec_m_new)
        REAL, DIMENSION(:), INTENT(IN) :: vec_t, vec_m, vec_t_new
        REAL, DIMENSION(:), INTENT(OUT) :: vec_m_new
        INTEGER :: n, i

        ! Data size
        n = SIZE(vec_t)

        ! Perform extrapolation
        DO i = 1, n
            IF (vec_t_new(i) <= vec_t(1) .OR. vec_t_new(i) >= vec_t(n)) THEN ! Case 1: Extrapolation for values outside of data range
                vec_m_new(i) = extrapolate_linear(vec_t, vec_m, vec_t_new(i))
            ELSE ! Case 2: Perform interpolation
                vec_m_new(i) = interpolate(vec_t, vec_m, vec_t_new(i))
            END IF
        END DO
    END SUBROUTINE linear_interpolation_linear_extrapolation
END MODULE linear_interpolation

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
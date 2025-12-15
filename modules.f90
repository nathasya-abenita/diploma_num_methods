!===========================================================
!   Module: Sorter
!===========================================================

module module_sorter
  implicit none
  private

  public :: quicksort

  contains

  recursive subroutine quicksort(a)
    real, dimension(:), intent(inout) :: a
    integer :: p
    if (size(a) == 0) return
    p = partition(a)
    call quicksort(a(:p-1))
    call quicksort(a(p+1:))
  end subroutine quicksort

  integer function partition(a) result(i)
    real, dimension(:), intent(inout) :: a
    integer :: j
    i = 1
    do j = 1, size(a)
        if (a(j) < a(size(a))) then
            call swap(a(i), a(j))
            i = i + 1
        end if
    end do
    call swap(a(i), a(size(a)))
  end function partition

  elemental subroutine swap(a, b)
    real, intent(inout) :: a, b
    real :: temp
    temp = a
    a = b
    b = temp
  end subroutine swap

end module module_sorter

!===========================================================
!   Module: Matrix Operator
!   Purpose: Basic functions relating to array and matrices
!===========================================================

MODULE matrix_operator
    USE module_sorter
    IMPLICIT NONE
CONTAINS
    REAL FUNCTION percentile(percentage, vec_input) RESULT(res)
        REAL, INTENT(IN) :: percentage
        REAL, DIMENSION(:), INTENT(IN) :: vec_input
        REAL, DIMENSION(:), ALLOCATABLE :: vec
        REAL :: idx
        INTEGER :: idx_int

        ! Sort vector
        ALLOCATE(vec(SIZE(vec_input)))
        vec = vec_input
        CALL quicksort(vec)

        ! Compute the index or position of the number
        idx = REAL(SIZE(vec)) * percentage / 100.0
        idx_int = INT(idx)

        ! Compute the percentile
        res = vec(idx_int) + (idx - idx_int * 1.0) * &
            (vec(idx_int + 1) - vec(idx_int))
        
        ! Deallocate for memory efficiency
        DEALLOCATE(vec)
    END FUNCTION percentile

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

    FUNCTION inverse_2d (mat) RESULT(mat_out)
        REAL, DIMENSION(2, 2), INTENT(IN) :: mat
        REAL, DIMENSION(2, 2) :: mat_out

        ! Construct adjoint matrix
        mat_out(1, 1) = mat(2, 2)
        mat_out(1, 2) = -1.0 * mat(1, 2)
        mat_out(2, 1) = -1.0 * mat(2, 1)
        mat_out(2, 2) = mat(1, 1)

        ! Final computation
        mat_out = mat_out / determinant(mat)
    END FUNCTION inverse_2d
END MODULE matrix_operator

!===========================================================
!   Module: Read file
!   Purpose: Basic file reading
!===========================================================

MODULE read_file
    IMPLICIT NONE
CONTAINS
    SUBROUTINE read_vec (file_name, vec, n_row)
        INTEGER :: i
        CHARACTER(LEN=*), INTENT(IN) :: file_name
        REAL, DIMENSION(:,:), ALLOCATABLE :: mat
        REAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: vec
        INTEGER :: n_col
        INTEGER, INTENT(OUT) :: n_row

        ! Initialize number of rows and columns
        n_col = 0
        n_row = 0

        ! Read data as matrix
        CALL read_mat(file_name, mat, n_col, n_row)

        ! Make sure it's a vector data
        IF (n_col > 1) STOP 'Data is not one-dimensional!'

        ! Save data as vector
        ALLOCATE(vec(n_row))
        DO i = 1, n_row
            vec(i) = mat(i, 1)
        END DO
    END SUBROUTINE

    SUBROUTINE read_mat (file_name, mat, n_col, n_row)
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
        ! f = x**5 + 3*x**4 - 2*x**2 - LOG(x**2) - 2
        ! f = COS(x) - LOG(x) + 1
        f = x**3 - LOG(x) - x - 4! x**2 - 6*x - LOG(x) - 4
    END FUNCTION func_f

    REAL FUNCTION func_f_prime (x) RESULT(f)
        REAL(rpm), INTENT(IN) :: x
        ! f = -1.0 * SIN(x) - (1.0 / x) 
        f = 3*x**2 - 1/x - 1 ! 2*x - 6 - 1/x
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
!   of extrapolation: linear or constant
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

!===========================================================
!   Module: Linear fit
!   Purpose: Perform linear fit for one dimensional function
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

!===========================================================
!   Module: Non-linear fit
!   Purpose: Perform non-linear fit with two dimensional
!   function
!===========================================================

MODULE nonlinear_fit
    USE matrix_operator
    USE linear_system
    IMPLICIT NONE
CONTAINS

    ! Regression model function
    REAL FUNCTION f (x, b1, b2) RESULT(res)
        REAL, INTENT(IN) :: x, b1, b2

        res = b1 / (x**5 * (EXP(b2/x) - 1))
    END FUNCTION f

    ! Partial derivative of the function to beta_1
    REAL FUNCTION df_db1 (x, b1, b2) RESULT(res)
        REAL, INTENT(IN) :: x, b1, b2

        res = 1 / (x**5 * (EXP(b2/x) - 1))
    END FUNCTION df_db1

    ! Partial derivative of the function to beta_2
    REAL FUNCTION df_db2 (x, b1, b2) RESULT(res)
        REAL, INTENT(IN) :: x, b1, b2

        res = -1.0 * (b1 * EXP(b2 / x)) &
            / (x**6 * (EXP(b2/x) - 1)**2)
    END FUNCTION df_db2 

    SUBROUTINE nonlinear_fit_2d (x, y, alpha, b1, b2, delta_b, se1, se2)
        REAL, DIMENSION(:), INTENT(IN) :: x, y
        REAL, INTENT(IN) :: alpha
        REAL, INTENT(INOUT) :: b1, b2
        REAL, INTENT(OUT) :: se1, se2
        REAL, DIMENSION(2), INTENT(INOUT) :: delta_b
        INTEGER :: n, i ! Data size and looping index
        REAL, DIMENSION(:, :), ALLOCATABLE :: jac ! Jacobian matrix
        REAL, DIMENSION(:), ALLOCATABLE :: r ! Residual vector
        REAL :: var ! Variance of error
        REAL, DIMENSION(2, 2) :: se ! Std. errors for fit parameters
        
        ! Data size
        n = SIZE(x)
        ALLOCATE(r(n))
        ALLOCATE(jac(n, 2))

        ! Construct residuals vector
        DO i = 1, n
            r(i) = y(i) - f(x(i), b1, b2)
        END DO

        ! Construct Jacobian matrix
        DO i = 1, n
            jac(i, 1) = df_db1(x(i), b1, b2)
            jac(i, 2) = df_db2(x(i), b1, b2)
        END DO

        ! Find descent direction
        CALL cramer_rule (MATMUL(TRANSPOSE(jac), jac), &
            MATMUL(TRANSPOSE(jac), r), &
            delta_b)

        ! Update fit parameters
        b1 = b1 + alpha * delta_b(1)
        b2 = b2 + alpha * delta_b(2) 

        ! Compute variance of error
        var = 0
        DO i = 1, n
            var = var + (y(i) - f(x(i), b1, b2)) ** 2
        END DO
        var = var / (n - 2)

        ! Compute standard errors of fit parameters
        se = var * inverse_2d(MATMUL(TRANSPOSE(jac), jac))

        ! Compute standard deviation
        se1 = SQRT(se(1, 1))
        se2 = SQRT(se(2, 2))
    END SUBROUTINE nonlinear_fit_2d
END MODULE nonlinear_fit

!===========================================================
!   Module: Integration
!   Purpose: Utilize trapezoidal and simpson methods
!===========================================================

MODULE integration
    IMPLICIT NONE
CONTAINS
    ! Function to be integrated

    REAL FUNCTION f (x) RESULT(res)
        REAL, INTENT(IN) :: x

        ! res = (16*x - 16) / (x**4 - 2*x**3 + 4*x - 4)
        res = SIN(x**2) ! SIN(x) * EXP(-1.0 * x)
    END FUNCTION f

    ! Gauss-Legendre quadrature method

    REAL FUNCTION approx_int_gauss_leg(a, b, n) RESULT(res)
        REAL, INTENT(IN) :: a, b ! Integration bounds
        INTEGER, INTENT(IN) :: n ! Number of partition
        REAL :: h ! Partition width
        REAL :: p1, p2 ! Weight
        INTEGER :: i ! Looping index

        ! Define weight
        p1 = 0.5 + 0.5 / SQRT(3.0)
        p2 = 0.5 - 0.5 / SQRT(3.0)

        ! Compute partition width
        h = (b - a) / n

        ! Apply trapezoidal formula
        res = 0
        DO i = 0, n - 1
            res = res + f(a + i*h + p1*h) + f(a + i*h + p2*h)
        END DO
        res = res * h / 2
    END FUNCTION approx_int_gauss_leg

    REAL FUNCTION int_gauss_leg(a, b, eps) RESULT(res)
        REAL, INTENT(IN) :: a, b, eps ! Integration parameters
        INTEGER :: n = 2 ! Initial value for number of partitions
        REAL :: res_old ! Old approximation

        ! Compute initial approximation of integration
        res_old = approx_int_gauss_leg(a, b, n)
        ! Loop until convergence is met
        DO
            ! Increase number of partitions
            n = 2*n

            ! Compute new approximation
            res = approx_int_gauss_leg(a, b, n)

            ! Print info
            PRINT*, '(N, integration) =', n, res, ABS(res - res_old)

            ! Exit loop when convergence is met
            IF (ABS(res - res_old) < eps) EXIT

            ! Update approximation
            res_old = res
        END DO
    END FUNCTION int_gauss_leg

    ! Trapezoidal method

    REAL FUNCTION approx_int_trapezoid(a, b, n) RESULT(res)
        REAL, INTENT(IN) :: a, b ! Integration bounds
        INTEGER, INTENT(IN) :: n ! Number of partition
        REAL :: h ! Partition width
        INTEGER :: i ! Looping index

        ! Compute partition width
        h = (b - a) / n

        ! Apply trapezoidal formula
        res = 0
        DO i = 0, n - 1
            res = res + f(a + i*h) + f(a + (i+1)*h)
        END DO 
        res = res * h / 2
    END FUNCTION approx_int_trapezoid

    REAL FUNCTION int_trapezoid(a, b, eps) RESULT(res)
        REAL, INTENT(IN) :: a, b, eps ! Integration parameters
        INTEGER :: n = 32 ! Initial value for number of partitions
        REAL :: res_old ! Old approximation

        ! Compute initial approximation of integration
        res_old = approx_int_trapezoid(a, b, n)
        ! Loop until convergence is met
        DO
            ! Increase number of partitions
            n = 2*n

            ! Compute new approximation
            res = approx_int_trapezoid(a, b, n)

            ! Print info
            PRINT*, '(N, integration) =', n, res, ABS(res - res_old)

            ! Exit loop when convergence is met
            IF (ABS(res - res_old) < eps) EXIT

            ! Update approximation
            res_old = res
        END DO
    END FUNCTION int_trapezoid

    ! Simpson method

    REAL FUNCTION approx_int_simpson(a, b, n) RESULT(res)
        REAL, INTENT(IN) :: a, b ! Integration bounds
        INTEGER, INTENT(IN) :: n ! Number of partition
        REAL :: h ! Partition width
        INTEGER :: i ! Looping index

        ! Compute partition width
        h = (b - a) / n

        ! Apply Simpson method formula
        res = 0
        DO i = 0, n - 1
            res = res + f(a + i*h) + f(a + (i+1)*h) + &
            4 * f((2*a + (2*i+1)*h)/2)
        END DO 
        res = res * h / 6
    END FUNCTION approx_int_simpson

    REAL FUNCTION int_simpson(a, b, eps) RESULT(res)
        REAL, INTENT(IN) :: a, b, eps ! Integration parameters
        INTEGER :: n = 2 ! Initial value for number of partitions
        REAL :: res_old ! Old approximation

        ! Compute initial approximation of integration
        res_old = approx_int_simpson(a, b, n)
        ! Loop until convergence is met
        DO
            ! Increase number of partitions
            n = 2*n

            ! Compute new approximation
            res = approx_int_simpson(a, b, n)

            ! Print info
            PRINT*, '(N, integration) =', n, res

            ! Exit loop when convergence is met
            IF (ABS(res - res_old) < eps) EXIT

            ! Update approximation
            res_old = res
        END DO
    END FUNCTION int_simpson
END MODULE integration

!===========================================================
!   Module: Solve differential equation - One-dimensional 
!   function
!   Purpose: 
!===========================================================


MODULE diff_equation
    IMPLICIT NONE
CONTAINS

    ! Functions corresponding to the diff. equation system

    REAL FUNCTION f(t, x) RESULT(res) ! Single system function
        REAL, INTENT(IN) :: t, x

        res = x ** 2 / (1 + t)
    END FUNCTION f

    REAL FUNCTION fx(t, x, y) RESULT(res)
        REAL, INTENT(IN) :: t, x, y ! Function input
        ! Define pi constant
        REAL :: pi
        pi = 4 * ATAN(1.0)
        res = x * SQRT(8.0 * pi / (3.0 * x ** 3) - 1 / x ** 2)
    END FUNCTION fx

    REAL FUNCTION fy(t, x, y) RESULT(res)
        REAL, INTENT(IN) :: t, x, y ! Function input

        res = 0 !res = -1.0 * SIN(x)
    END FUNCTION fy

    ! Euler method

    SUBROUTINE euler_method_single_system (t0, x0, delta_t, n, t, x)
        REAL, INTENT(IN) :: t0, x0, delta_t
        INTEGER, INTENT(IN) :: n
        REAL, DIMENSION(:), INTENT(OUT) :: t, x ! Dimension is n + 1
        INTEGER :: i ! Looping index

        ! Set initial condition
        x(1) = x0
        t(1) = t0

        ! Apprximate next steps with Euler method
        DO i = 1, n ! Repeat N times
            t(i + 1) = t(i) + delta_t
            x(i + 1) = x(i) + delta_t * f(t(i), x(i))
        END DO
    END SUBROUTINE euler_method_single_system

    SUBROUTINE euler_method_1d (t0, x0, y0, delta_t, n, t, x, y)
        REAL, INTENT(IN) :: t0, x0, y0, delta_t
        INTEGER, INTENT(IN) :: n
        REAL, DIMENSION(:), INTENT(OUT) :: t, x, y ! Dimension is n + 1
        INTEGER :: i ! Looping index

        ! Set initial condition
        x(1) = x0
        y(1) = y0
        t(1) = t0

        ! Apprximate next steps with Euler method
        DO i = 1, n ! Repeat N times
            t(i + 1) = t(i) + delta_t
            x(i + 1) = x(i) + delta_t * fx(t(i), x(i), y(i))
            y(i + 1) = y(i) + delta_t * fy(t(i), x(i), y(i))
        END DO
    END SUBROUTINE euler_method_1d

    ! Verlet method

    SUBROUTINE verlet_method_1d (t0, x0, y0, delta_t, n, t, x, y)
        REAL, INTENT(IN) :: t0, x0, y0, delta_t
        INTEGER, INTENT(IN) :: n
        REAL, DIMENSION(:), INTENT(OUT) :: t, x, y ! Dimension is n + 1
        INTEGER :: i ! Looping index

        ! Set initial condition
        x(1) = x0
        y(1) = y0
        t(1) = t0

        ! Approximate next steps with Verlet method
        DO i = 1, n ! Repeat N times
            t(i + 1) = t(i) + delta_t
            y(i + 1) = y(i) + 0.5 * delta_t * fy(t(i), x(i), y(i))
            x(i + 1) = x(i) + delta_t * y(i + 1)
            y(i + 1) = y(i + 1) + 0.5 * delta_t * fy(t(i), x(i + 1), y(i + 1))            
        END DO
    END SUBROUTINE verlet_method_1d

    ! TWO-DIMENSIONAL FUNCTION

    ! Functions corresponding to the diff. equation system

    REAL FUNCTION fx1(t, x1, x2, y1, y2) RESULT(res)
        REAL, INTENT(IN) :: t, x1, x2, y1, y2 ! Function input
        res = y1
    END FUNCTION fx1

    REAL FUNCTION fx2(t, x1, x2, y1, y2) RESULT(res)
        REAL, INTENT(IN) :: t, x1, x2, y1, y2 ! Function input
        res = y2
    END FUNCTION fx2

    REAL FUNCTION fy1(t, x1, x2, y1, y2) RESULT(res)
        REAL, INTENT(IN) :: t, x1, x2, y1, y2 ! Function input
        res = -1.0 * x1 / (SQRT(x1**2 + x2**2) ** 3)
    END FUNCTION fy1

    REAL FUNCTION fy2(t, x1, x2, y1, y2) RESULT(res)
        REAL, INTENT(IN) :: t, x1, x2, y1, y2 ! Function input
        res = -1.0 * x2 / (SQRT(x1**2 + x2**2) ** 3)
    END FUNCTION fy2

    ! Euler method

    SUBROUTINE euler_method_2d (t0, x0, y0, delta_t, n, t, x, y)
        REAL, INTENT(IN) :: t0, delta_t
        REAL, DIMENSION(:), INTENT(IN) :: x0, y0
        INTEGER, INTENT(IN) :: n
        REAL, DIMENSION(:), INTENT(OUT) :: t
        REAL, DIMENSION(:, :), INTENT(OUT) :: x, y
        INTEGER :: i ! Looping index

        ! Set initial condition
        x(1, 1) = x0(1)
        x(2, 1) = x0(2)
        y(1, 1) = y0(1)
        y(2, 1) = y0(2)
        t(1) = t0

        ! Apprximate next steps with Euler method
        DO i = 1, n ! Repeat N times
            t(i + 1) = t(i) + delta_t

            x(1, i + 1) = x(1, i) + delta_t * fx1(t(i), x(1, i), x(2, i), y(1, i), y(2, i))
            x(2, i + 1) = x(2, i) + delta_t * fx2(t(i), x(1, i), x(2, i), y(1, i), y(2, i))

            y(1, i + 1) = y(1, i) + delta_t * fy1(t(i), x(1, i), x(2, i), y(1, i), y(2, i))
            y(2, i + 1) = y(2, i) + delta_t * fy2(t(i), x(1, i), x(2, i), y(1, i), y(2, i))
        END DO
    END SUBROUTINE euler_method_2d

    ! Verlet method

    SUBROUTINE verlet_method_2d (t0, x0, y0, delta_t, n, t, x, y)
        REAL, INTENT(IN) :: t0, delta_t
        REAL, DIMENSION(:), INTENT(IN) :: x0, y0
        INTEGER, INTENT(IN) :: n
        REAL, DIMENSION(:), INTENT(OUT) :: t
        REAL, DIMENSION(:, :), INTENT(OUT) :: x, y
        INTEGER :: i ! Looping index

        ! Set initial condition
        x(1, 1) = x0(1)
        x(2, 1) = x0(2)
        y(1, 1) = y0(1)
        y(2, 1) = y0(2)
        t(1) = t0

        ! Approximate next steps with Verlet method
        DO i = 1, n ! Repeat N times
            t(i + 1) = t(i) + delta_t

            y(1, i + 1) = y(1, i) + 0.5 * delta_t * fy1(t(i), x(1, i), x(2, i), y(1, i), y(2, i))
            y(2, i + 1) = y(2, i) + 0.5 * delta_t * fy2(t(i), x(1, i), x(2, i), y(1, i), y(2, i))

            x(1, i + 1) = x(1, i) + delta_t * y(1, i + 1)
            x(2, i + 1) = x(2, i) + delta_t * y(2, i + 1)

            y(1, i + 1) = y(1, i + 1) + 0.5 * delta_t * fy1(t(i), x(1, i + 1), x(2, i + 1), y(1, i + 1), y(2, i + 1))
            y(2, i + 1) = y(2, i + 1) + 0.5 * delta_t * fy2(t(i), x(1, i + 1), x(2, i + 1), y(1, i + 1), y(2, i + 1))            
        END DO
    END SUBROUTINE verlet_method_2d
END MODULE diff_equation

!===========================================================
!   Module: Random numbers simulation
!   Purpose: Generate from certain distribution
!===========================================================

MODULE random_numbers
    USE matrix_operator
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

    REAL FUNCTION inverse_cdf_1 (x) RESULT(res)
        REAL, INTENT(IN) :: x

        res = -1 - 2 / (x - 2)
    END FUNCTION inverse_cdf_1

    REAL FUNCTION inverse_cdf_2 (x) RESULT(res)
        REAL, INTENT(IN) :: x

        res = x ** (1.0 / 3)
    END FUNCTION inverse_cdf_2

    SUBROUTINE apply_box_muller (z1, z2, m, s)
        REAL, DIMENSION(:), INTENT(INOUT) :: z1, z2
        REAL, DIMENSION(:), ALLOCATABLE :: u1, u2
        REAL, INTENT(IN) :: m, s ! Mean and variance
        INTEGER :: i ! Looping index
        REAL :: pi ! Pi number

        pi = 4.0 * ATAN(1.0)

        ! Allocate array based on size input
        ALLOCATE(u1(SIZE(z1)))
        ALLOCATE(u2(SIZE(z1)))

        ! Generate uniform numbers
        CALL RANDOM_NUMBER(u1) 
        CALL RANDOM_NUMBER(u2)

        ! Apply Box-Muller method
        DO i = 1, SIZE(z1)
            z1(i) = SQRT( -2 * LOG(u1(i)) ) * COS(2 * pi * u2(i))
            z2(i) = SQRT( -2 * LOG(u1(i)) ) * SIN(2 * pi * u2(i))
        END DO

        ! Distribution shift
        z1 = SQRT(s) * z1 + m
        z2 = SQRT(s) * z2 + m

    END SUBROUTINE apply_box_muller

    SUBROUTINE write_output (dat, file_name)
        REAL, DIMENSION(:), INTENT(IN) :: dat
        CHARACTER(LEN=*), INTENT(IN) :: file_name

        OPEN (UNIT=11, FILE=file_name, STATUS='replace', ACTION='write')
            WRITE(11, *) dat
        CLOSE(11)
    END SUBROUTINE write_output
END MODULE random_numbers

MODULE monte_carlo
    USE random_numbers ! for gen_uniform
    IMPLICIT NONE
CONTAINS

    ! FUNCTIONS

    REAL FUNCTION func_f (x) RESULT(res) ! For rejection method
        REAL, INTENT(IN) :: x

        res = EXP(-1.0 * x)
    END FUNCTION func_f

    REAL FUNCTION func_g (x) RESULT(res) ! For crude Monte-Carlo method
        REAL, INTENT(IN) :: x

        res = x * EXP(-1.0 * x ** 2.0)
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
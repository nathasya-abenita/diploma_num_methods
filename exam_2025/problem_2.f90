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
        f = 3*x - COS(x) - 1
    END FUNCTION func_f

    REAL FUNCTION func_f_prime (x) RESULT(f)
        REAL(rpm), INTENT(IN) :: x
        f = 3 + SIN(x)
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

PROGRAM find_root
    use root_finder
    IMPLICIT NONE
    INTEGER, PARAMETER :: rp = 8
    REAL(rp) :: a, b, eps, single_guess

    ! Define inputs (brackets, precision, and initial guess)
    a = 0
    b = 1
    eps = 1E-4
    single_guess = 0.5

    ! Call methods
    CALL perform_all_methods(a, b, eps, single_guess)

    ! Print conclusion
    PRINT*, 'Conclusion: the intersection is at x = 0.6071'
END PROGRAM find_root


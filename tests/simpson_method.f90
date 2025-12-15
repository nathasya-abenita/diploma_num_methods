PROGRAM compute_integration
    USE integration
    IMPLICIT NONE
    REAL :: a, b, eps, res ! Integration parameters

    ! Set parameters
    a = 0 ! lower bound
    b = SQRT(2 * 4 * ATAN(1.0)) ! upper bound
    eps = 1e-4 ! threshold

    ! Compute integration and print results
    PRINT*, 'Integration result will be printed...'
    PRINT*, 'With trapezoidal method:'
    res = int_trapezoid(a, b, eps)
    PRINT*, 'Final result:', res

    ! Compute integration and print results
    PRINT*, 'With Simpson method:'
    res = int_simpson(a, b, eps)
    PRINT*, 'Final result:', res

    ! Compute integration and print results
    PRINT*, 'With Gauss-Legendre method:'
    res = int_gauss_leg(a, b, eps)
    PRINT*, 'Final result:', res
END PROGRAM compute_integration
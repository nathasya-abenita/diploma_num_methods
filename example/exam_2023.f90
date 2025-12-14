PROGRAM exam_2023
    USE root_finder
    USE integration
    IMPLICIT NONE
    INTEGER, PARAMETER :: rp = 4
    REAL(rp) :: a, b, eps, single_guess, res


    ! SECOND PROBLEM (Make sure rp = 8)

    ! ! Define inputs
    ! a = 6
    ! b = 8
    ! eps = 1E-4
    ! single_guess = 7
    ! PRINT*, 'Searching for first root...'
    ! CALL perform_all_methods(a, b, eps, single_guess)

    ! THIRD PROBLEM
    a = 0
    b = 1
    eps = 1E-5
    res = int_gauss_leg(a, b, eps)
    PRINT*, 'Gauss-Legendre method : \int_0^1{f(x)} dx =', res

    res = int_trapezoid(a, b, eps)
    PRINT*, 'Trapozoidal method : \int_0^1{f(x)} dx =', res

END PROGRAM exam_2023
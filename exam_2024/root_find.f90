PROGRAM root_find
    use root_finder
    IMPLICIT NONE
    INTEGER, PARAMETER :: rp = 8
    REAL(rp) :: a, b, eps, single_guess


    ! Define inputs
    a = 1
    b = 2
    eps = 1E-5
    single_guess = 1.5

    ! Call methods
    CALL perform_all_methods(a, b, eps, single_guess)
END PROGRAM root_find
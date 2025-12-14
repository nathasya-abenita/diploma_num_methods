PROGRAM root_find
    use root_finder
    IMPLICIT NONE
    INTEGER, PARAMETER :: rp = 8
    REAL(rp) :: a, b, eps, single_guess


    ! FIRST ROOT (Main problem from the assignment)

    ! Define inputs
    a = 1
    b = 3
    eps = 1E-7
    single_guess = 1.0
    PRINT*, 'Searching for first root...'
    CALL perform_all_methods(a, b, eps, single_guess)

    ! SECOND ROOT 

    ! Define inputs
    a = 5
    b = 6
    eps = 1E-7
    single_guess = 5.0
    PRINT*, 'Searching for second root...'
    CALL perform_all_methods(a, b, eps, single_guess)

    ! THIRD ROOT 

    ! Define inputs
    a = 6.5
    b = 8
    eps = 1E-7
    single_guess = 7.0
    PRINT*, 'Searching for third root...'
    CALL perform_all_methods(a, b, eps, single_guess)

END PROGRAM root_find

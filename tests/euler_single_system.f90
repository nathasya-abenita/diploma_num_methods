PROGRAM solve_diff_equation
    USE diff_equation
    IMPLICIT NONE
    REAL :: t0, x0, tf, delta_t ! Initial conditions
    INTEGER :: n, i ! Number of iterations, loop index
    REAL, DIMENSION(:), ALLOCATABLE :: t, x ! Arrays of solution
    INTEGER :: unit_num = 11 ! Unit number to access file
    CHARACTER(LEN=11) :: num_format = '(2F10.4)'
    CHARACTER(LEN=11) :: head_format = '(2A10)'

    ! Define initial conditions
    t0 = 0.0
    x0 = -1.0
    
    tf = 1

    ! Calculate number of iterations
    n = 19
    delta_t = (tf - t0) / n
    
    ! Preparing output arrays
    ALLOCATE(t(n + 1), x(n + 1))

    ! Perform Euler method
    CALL euler_method_single_system (t0, x0, delta_t, n, t, x)

    ! Write the output
    OPEN(UNIT=unit_num, FILE='./output/euler_single_system.txt', STATUS='replace', ACTION='write')
        WRITE(unit_num, head_format) 't', 'y(t)'
        DO i = 1, n + 1
            WRITE(unit_num, num_format) t(i), x(i)
        END DO
    CLOSE(unit_num)
        
END PROGRAM solve_diff_equation
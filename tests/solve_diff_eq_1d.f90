PROGRAM solve_diff_equation
    USE diff_equation
    IMPLICIT NONE
    REAL :: t0, x0, y0, tf, delta_t ! Initial conditions
    INTEGER :: n, i ! Number of iterations, loop index
    REAL, DIMENSION(:), ALLOCATABLE :: t, x, y ! Arrays of solution
    INTEGER :: unit_num = 11 ! Unit number to access file
    CHARACTER(LEN=11) :: num_format = '(3F10.4)'
    CHARACTER(LEN=11) :: head_format = '(3A10)'

    ! Define initial conditions
    t0 = 0
    x0 = 1
    y0 = 0
    delta_t = 0.1
    tf = 30

    ! Calculate number of iterations
    n = INT((tf - t0) / delta_t)
    
    ! Preparing output arrays
    ALLOCATE(t(n + 1), x(n + 1), y(n + 1))

    ! Perform Euler method
    CALL euler_method_1d (t0, x0, y0, delta_t, n, t, x, y)

    ! Write the output
    OPEN(UNIT=unit_num, FILE='./output/euler1.txt', STATUS='replace', ACTION='write')
        WRITE(unit_num, head_format) 't', 'x(t)', 'y(t)'
        DO i = 1, n + 1
            WRITE(unit_num, num_format) t(i), x(i), y(i)
        END DO
    CLOSE(unit_num)

    ! Perform Verlet method
    CALL verlet_method_1d (t0, x0, y0, delta_t, n, t, x, y)

    ! Write the output
    OPEN(UNIT=unit_num, FILE='./output/verlet1.txt', STATUS='replace', ACTION='write')
        WRITE(unit_num, head_format) 't', 'x(t)', 'v(t)'
        DO i = 1, n + 1
            WRITE(unit_num, num_format) t(i), x(i), y(i)
        END DO
    CLOSE(unit_num)
        
END PROGRAM solve_diff_equation
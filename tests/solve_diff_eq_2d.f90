PROGRAM solve_diff_equation
    USE diff_equation
    IMPLICIT NONE
    REAL :: t0, tf, delta_t ! Initial conditions
    REAL, DIMENSION(2) :: x0, y0
    INTEGER :: n, i ! Number of iterations, loop index
    REAL, DIMENSION(:), ALLOCATABLE :: t ! Arrays of solution
    REAL, DIMENSION(:, :), ALLOCATABLE :: x, y ! Arrays of solution
    INTEGER :: unit_num = 11 ! Unit number to access file
    CHARACTER(LEN=11) :: num_format = '(5F10.4)'
    CHARACTER(LEN=11) :: head_format = '(5A10)'

    ! Define initial conditions
    t0 = 0
    x0 = (/ 2.0, 0.0 /)
    y0 = (/ 0.0, 0.5 /)
    delta_t = 0.001
    tf = 30

    ! Calculate number of iterations
    n = INT((tf - t0) / delta_t)
    
    ! Preparing output arrays
    ALLOCATE(t(n + 1), x(2, n + 1), y(2, n + 1))

    ! Perform Euler method
    CALL euler_method_2d (t0, x0, y0, delta_t, n, t, x, y)

    ! Write the output
    OPEN(UNIT=unit_num, FILE='./output/euler.txt', STATUS='replace', ACTION='write')
        WRITE(unit_num, head_format) 't', 'x1(t)', 'x2(t)', 'y1(t)', 'y2(t)'
        DO i = 1, n + 1
            WRITE(unit_num, num_format) t(i), x(1, i), x(2, i), y(1, i), y(2, i)
        END DO
    CLOSE(unit_num)

    ! Perform Verlet method
    CALL verlet_method_2d (t0, x0, y0, delta_t, n, t, x, y)

    ! Write the output
    OPEN(UNIT=unit_num, FILE='./output/verlet.txt', STATUS='replace', ACTION='write')
        WRITE(unit_num, head_format) 't', 'x1(t)', 'x2(t)', 'y1(t)', 'y2(t)'
        DO i = 1, n + 1
            WRITE(unit_num, num_format) t(i), x(1, i), x(2, i), y(1, i), y(2, i)
        END DO
    CLOSE(unit_num)
        
END PROGRAM solve_diff_equation
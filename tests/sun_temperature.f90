PROGRAM sun_surface_temp_estimation
    USE nonlinear_fit
    IMPLICIT NONE
    INTEGER, PARAMETER :: n = 1612 ! Data size
    REAL :: alpha, eps ! Regression parameters
    INTEGER :: maxIter
    REAL, DIMENSION(n) :: y, x ! To store input data (independent and response variables)
    INTEGER :: i, iter ! Looping index and performed number of iteration
    INTEGER :: unit_num = 11 ! Unit number for accessing files
    REAL :: b1, b2, se1, se2  ! Fit parameters and the standard errors
    REAL, DIMENSION(2) :: delta_b ! Descent direction vector
    CHARACTER(LEN=11) :: num_format = '(4F10.4)'
    CHARACTER(LEN=11) :: head_format = '(4A10)'

    ! Define regression parameters
    alpha = 0.1
    eps = 1e-6
    maxIter = 1000

    ! Initialize fit parameters
    b1 = 1.0
    b2 = 1.0

    ! Open input data file
    OPEN(UNIT=unit_num, FILE='./data_sample/sun_data.txt', STATUS='old', ACTION='read')

    ! Read empty line(s)
    READ(unit_num, *)
    READ(unit_num, *)

    ! Read data
    DO i = 1, n
        READ(unit_num, *) x(i), y(i)
    END DO
    CLOSE(unit_num) ! Close data reading

    ! Open output file and write header
    OPEN(UNIT=unit_num, FILE='./output/fit_sun.txt', STATUS='replace', ACTION='write')
    WRITE(unit_num, head_format) 'B1', 'B2', 'SE(B1)', 'SE(B2)' 

    ! Perform fit with linear regression
    DO i = 1, maxIter
        ! Update parameter fit
        CALL nonlinear_fit_2d (x, y, alpha, b1, b2, delta_b, se1, se2)

        ! Perform quality fit

        ! Write output
        WRITE(unit_num, num_format) b1, b2, se1, se2

        ! Stop if the descent is below threshold
        IF ((delta_b(1) ** 2 + delta_b(2) ** 2) <= eps) EXIT
    END DO 

    ! Close output file
    CLOSE(unit_num)

    ! Compute the final temperature
    PRINT*, 'The temperature of the sun (in K) is', (14387.77) / b2
END PROGRAM sun_surface_temp_estimation
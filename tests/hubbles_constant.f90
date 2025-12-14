
PROGRAM compute_hubbles_constant
    USE linear_fit
    IMPLICIT NONE
    INTEGER, PARAMETER :: n_size = 2 ! Matrix size (can be 2 or 3)
    REAL, DIMENSION(15) :: d, v ! To store data
    INTEGER :: i ! Looping index
    INTEGER :: unit_num = 11 ! Unit number for accessing files
    REAL :: b1, b2, r_sq, std_1, std_2 ! Fit parameters, the coef. of determination, and std. err.
    CHARACTER(LEN=11) :: str_format = '(A15 F10.2)'

    ! Open input data file
    OPEN(UNIT=unit_num, FILE='./data_sample/hubble_data.txt', STATUS='old', ACTION='read')

    ! Read empty line
    READ(unit_num, *)

    ! Read data
    DO i = 1, 15
        READ(unit_num, *) v(i), d(i)
    END DO
    CLOSE(unit_num) ! Close data reading

    ! Perform fit with linear regression
    CALL linear_fit_2d (d, v, b1, b2)
    CALL quality_fit (d, v, b1, b2, std_1, std_2, r_sq)

    ! Write output
    OPEN(UNIT=unit_num, FILE='./output/fit.txt', STATUS='replace', ACTION='write')
    WRITE(unit_num, str_format) 'beta_1', b1
    WRITE(unit_num, str_format) 'beta_2', b2
    WRITE(unit_num, str_format) 'R^2', r_sq
    WRITE(unit_num, str_format) 'std for beta_1', std_1
    WRITE(unit_num, str_format) 'std for beta_2', std_2
    CLOSE(unit_num)

    ! (Bonus) Perform fit with linear regression without intercept
    PRINT*, 'Without intercept, the parameter fit (b) is', SUM(v * d) / SUM(d ** 2)
END PROGRAM compute_hubbles_constant
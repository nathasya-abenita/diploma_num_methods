PROGRAM perform_interpolation
    USE linear_interpolation
    IMPLICIT NONE
    REAL, DIMENSION(100) :: vec_t, vec_m ! Vectors for time and measurement
    REAL, DIMENSION(10) :: vec_t_new, vec_m_new, vec_m1_new ! Vectors for new time and interpolated measurements for two cases
    INTEGER :: unit_num = 11
    INTEGER :: i ! Loop index variable
    
    ! Define values for new time to be interpolated
    DO i = 1, 10
        vec_t_new(i) = 1 + (i - 1) * 11
    END DO

    ! Read input_data.dat and save to time and measurement vectors
    OPEN(UNIT=unit_num, FILE='./data_sample/input_data.dat', STATUS='old', ACTION='read')
    DO i = 1, 100
        READ(unit_num, '(f6.2, f7.2)') vec_t(i), vec_m(i)
    END DO
    CLOSE(unit_num)

    ! Compute the linear interpolated values for input using CONSTANT value extrapolation
    CALL linear_interpolation_constant_extrapolation (vec_t, vec_m, vec_t_new, vec_m_new)

    ! Write the output as output_dat.dat
    OPEN(UNIT=unit_num, FILE='./output/output_dat.dat', STATUS='replace', ACTION='write')
    DO i = 1, 10
        WRITE(unit_num, '(f6.2, f7.2)') vec_t_new(i), vec_m_new(i) 
    END DO
    CLOSE(unit_num)

    ! Read input_data1.dat
    OPEN(UNIT=unit_num, FILE='./data_sample/input_data1.dat', STATUS='old', ACTION='read')
    DO i = 1, 100
        READ(unit_num, *) vec_t(i), vec_m(i)
    END DO
    CLOSE(unit_num)

    ! Compute the linear interpolated values for input using LINEAR extrapolation
    CALL linear_interpolation_linear_extrapolation (vec_t, vec_m, vec_t_new, vec_m_new)

    ! Write the output as output_dat.dat
    OPEN(UNIT=unit_num, FILE='./output/output_dat1.dat', STATUS='replace', ACTION='write')
    DO i = 1, 10
        WRITE(unit_num, '(f6.2, f7.2)') vec_t_new(i), vec_m_new(i) 
    END DO
    CLOSE(unit_num)
END PROGRAM perform_interpolation
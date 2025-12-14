PROGRAM generate_random_numbers
    USE random_numbers
    IMPLICIT NONE
    INTEGER :: n ! Random numbers to be generated
    INTEGER :: i ! Looping index
    REAL, DIMENSION(:), ALLOCATABLE :: dat
    REAL, DIMENSION(:), ALLOCATABLE :: z1, z2

    ! 1. Generate from the first distribution

    ! Define number of random numbers
    n = INT(1E4)

    ! Allocate the output array
    ALLOCATE(dat(n))

    ! Generate uniform numbers
    CALL RANDOM_NUMBER(dat) 

    ! Transform to wanted distribution
    DO i = 1, n 
        dat(i) = inverse_cdf_1(dat(i))
    END DO

    ! Write output, average, and deallocate the output array
    CALL write_output(dat, './output/data_1.txt')
    PRINT*, 'Average for first problem:', average(dat)
    DEALLOCATE(dat)

    ! 2. Generate from the second distribution

    ! Define number of random numbers
    n = INT(1E6)

    ! Allocate the output array
    ALLOCATE(dat(n))

    ! Generate uniform numbers
    CALL RANDOM_NUMBER(dat) 

    ! Transform to wanted distribution
    DO i = 1, n 
        dat(i) = inverse_cdf_2(dat(i))
    END DO

    ! Write output and deallocate the output array
    CALL write_output(dat, './output/data_2.txt')
    PRINT*, 'Average for second problem:', average(dat)
    DEALLOCATE(dat)

    ! 3. Generate from Box-Muller method
    n = INT(1E4)

    ! Allocate output
    ALLOCATE(z1(n))
    ALLOCATE(z2(n))

    ! Apply Box-Muller method
    CALL apply_box_muller(z1, z2, 0.0, 1.0)
    
    ! Write output
    CALL write_output(z1, './output/z1.txt')
    CALL write_output(z2, './output/z2.txt')
    
    ! 4. Generate from Box-Muller method

    ! Apply Box-Muller method
    CALL apply_box_muller(z1, z2, 1.0, 4.0)

    ! Write output
    CALL write_output(z1, './output/z1_bonus.txt')
    CALL write_output(z2, './output/z2_bonus.txt')
END PROGRAM generate_random_numbers
PROGRAM file_average
    USE matrix_operator
    USE read_file
    IMPLICIT NONE
    INTEGER :: unit_num = 11
    INTEGER :: i
    INTEGER :: n_col ! Number of columns
    INTEGER :: n_row! Number of rows
    REAL, DIMENSION(:, :), ALLOCATABLE :: mat
    REAL, DIMENSION(:), ALLOCATABLE :: output

    ! Read file
    file_name = './data_sample/data.txt'
    CALL read_flexible(file_name, mat, n_col, n_row)

    ! Compute output
    ALLOCATE(output(n_col))
    DO i = 1, n_col
        output(i) = average(mat(:, i))
    END DO

    ! Write out file
    OPEN(UNIT=unit_num, FILE='./output/result.txt', STATUS='replace', ACTION='write')
    WRITE(unit_num, *) output
    CLOSE(unit_num)
END PROGRAM file_average
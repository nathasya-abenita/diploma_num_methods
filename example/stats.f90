PROGRAM stats
    USE module_sorter
    USE matrix_operator
    USE read_file
    IMPLICIT NONE
    REAL, DIMENSION(:, :), ALLOCATABLE :: mat ! Variable to save data
    REAL, DIMENSION(:), ALLOCATABLE :: vec ! Variable to save data
    REAL :: avg, std, per_98 ! Variable for statistics
    INTEGER :: i, per_98_idx_int, err_read ! Indexing variable
    INTEGER :: n_row, n_col
    INTEGER :: unit_num = 11 ! Unit number for opening file
    CHARACTER(LEN=80) :: file_name

    ! 1. Read data as matrix
    file_name = './data_sample/sample.dat'
    CALL read_flexible(file_name, mat, n_col, n_row)

    ! Store data in vector
    ALLOCATE(vec(n_row))
    DO i = 1, n_row
        vec(i) = mat(i, 1)
    END DO

    ! Compute mean
    avg = average(vec)

    ! Compute standard deviation
    std = standard_deviation(vec)

    ! Compute 98-th percentile
    per_98 = percentile(98.0, vec)

    ! Write output, by following requested format
    OPEN(UNIT=unit_num, FILE='./output/stats.dat', STATUS='replace', ACTION='write')
    WRITE(unit_num, *) '# THIS IS COMPUTED FROM SAMPLE/DAT'
    WRITE(unit_num, '(3a10)') 'MEAN', 'STD', '98PCT' 
    WRITE(unit_num, '(3f10.3)') avg, std, per_98
    CLOSE(unit_num)

    ! Write output, by following requested format
    OPEN(UNIT=unit_num, FILE='./output/stats_2023.dat', STATUS='replace', ACTION='write')
    WRITE(unit_num, *) '# THIS IS COMPUTED FROM SAMPLE/DAT'
    WRITE(unit_num, '(3a10)') 'MEAN', 'VAR', '90PCT' 
    WRITE(unit_num, '(3f10.3)') avg, std ** 2, percentile(90.0, vec)
    CLOSE(unit_num)
END PROGRAM stats
PROGRAM stats
    USE module_sorter
    USE matrix_operator
    USE read_file
    IMPLICIT NONE
    REAL, DIMENSION(:, :), ALLOCATABLE :: mat ! Variable to save data
    REAL, DIMENSION(:), ALLOCATABLE :: vec ! Variable to save data
    REAL :: avg, std ! Variable for statistics
    INTEGER :: i
    INTEGER :: n_row, n_col
    INTEGER :: unit_num = 11 ! Unit number for opening file
    CHARACTER(LEN=80) :: file_name

    ! Read data as matrix
    file_name = './exam_2024/samples.dat'
    CALL read_mat(file_name, mat, n_col, n_row)
    ALLOCATE(vec(n_row))

    ! Write output, by following requested format
    OPEN(UNIT=unit_num, FILE='./exam_2024/stats.txt', STATUS='replace', ACTION='write')
    WRITE(unit_num, *) '# THIS IS COMPUTED FROM SAMPLE/DAT'
    WRITE(unit_num, '(3a10)') 'MEAN', 'VAR'
        DO i = 1, n_col
            ! Prepare vector of data
            vec = mat(:, i)

            ! Compute mean
            avg = average(vec)

            ! Compute standard deviation
            std = standard_deviation(vec)

            ! Write output
            WRITE(unit_num, '(2f10.3)') avg, std**2
        END DO
    CLOSE(unit_num)
END PROGRAM stats
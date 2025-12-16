
!===========================================================
!   Module: Matrix Operator
!   Purpose: Basic functions relating to array and matrices
!===========================================================

MODULE matrix_operator
    USE module_sorter
    IMPLICIT NONE
CONTAINS
    REAL FUNCTION average(arr) RESULT(avg)
        REAL, DIMENSION (:), INTENT(IN) :: arr ! one-dimensional input
        INTEGER :: n ! Size variable
        INTEGER :: i ! indexing variable

        ! Take size of the array
        n = SIZE(arr) 
        
        ! Apply average formula
        avg = SUM(arr) / n
    END FUNCTION average

    REAL FUNCTION standard_deviation(arr) RESULT(std)
        REAL, DIMENSION (:), INTENT(IN) :: arr
        INTEGER :: n 

        ! Take size of the array
        n = SIZE(arr)

        ! Apply standard deviation formula (using the biased estimator)
        std = ( SUM( (arr - average(arr)) ** 2 ) / n ) ** 0.5
    END FUNCTION standard_deviation
END MODULE matrix_operator

!===========================================================
!   Module: Read file
!   Purpose: Basic file reading
!===========================================================

MODULE read_file
    IMPLICIT NONE
CONTAINS
    SUBROUTINE read_mat (file_name, mat, n_col, n_row)
        CHARACTER(LEN=1) :: first_char
        CHARACTER(LEN=80) :: line
        INTEGER :: unit_num = 11
        INTEGER :: i, err, n_trim ! Counter variables
        INTEGER :: row_start = 0 ! Starting line
        
        CHARACTER(LEN=*), INTENT(IN) :: file_name
        REAL, DIMENSION(:, :), ALLOCATABLE, INTENT(INOUT) :: mat
        INTEGER, INTENT(OUT) :: n_col, n_row

        ! Initialize number of rows and columns
        n_col = 0
        n_row = 0

        ! Open file
        OPEN(UNIT=unit_num, FILE=file_name, STATUS='old', ACTION='read')

        ! Reading number of comment lines in header
        DO
            READ(unit_num, *, iostat=err) first_char

            IF (first_char == '#') THEN
                row_start = row_start + 1
            ELSE
                EXIT
            END IF
        END DO
        PRINT*, 'Start line (without header):', row_start

        ! Reading number of rows, ignoring hash symbol, and saving information
        REWIND(unit_num)
        DO
            READ(unit_num, *, iostat=err) first_char
            IF (err /= 0) EXIT
            IF (first_char /= '#') n_row = n_row + 1
        END DO
        PRINT*, 'Number of rows:', n_row
        
        ! Reading number of columns
        REWIND(unit_num)
        READ(unit_num, '(A)') line
        n_trim = LEN_TRIM(line) ! Remove trailing whitespace
        DO i = 1, n_trim - 1
            IF (line(i: i+1) == '  ') n_col = n_col + 1 ! Find number of seperator
        END DO
        PRINT*, 'Number of col.:', n_col
        
        ! Define data variable size
        ALLOCATE(mat(n_row, n_col))

        ! Prepare to read file by skipping header lines
        REWIND(unit_num)
        IF (row_start > 0) THEN
            DO i = 1, row_start
                READ(unit_num, *)
            END DO
        END IF

        ! Read file
        DO i = 1, n_row
            READ(unit_num, *) mat(i, :)
        END DO

        ! Close file    
        CLOSE(unit_num)
    END SUBROUTINE
END MODULE read_file

PROGRAM stats
    USE matrix_operator
    USE read_file
    IMPLICIT NONE
    REAL, DIMENSION(:, :), ALLOCATABLE :: mat ! Variable to save data
    INTEGER :: i ! Loop index
    INTEGER :: n_row, n_col
    INTEGER :: unit_num = 11 ! Unit number for opening file
    CHARACTER(LEN=80) :: file_name

    REAL, DIMENSION(:), ALLOCATABLE :: vec_diff
    REAL :: avg, std

    ! Define file name
    file_name = 'data.txt'

    ! Read data, saving it to matrix
    ! (each column is a vector of the data)
    CALL read_mat(file_name, mat, n_col, n_row)

    ! Allocate output vectors
    ALLOCATE(vec_diff(n_row))

    ! Compute difference between column 3 and column 2
    vec_diff = ABS(mat(:, 3) - mat(:, 2))

    ! Compute average and standard deviation of the new array
    avg = average(vec_diff)
    std = standard_deviation(vec_diff)

    ! Write output of stats
    PRINT*, 'Stats are printed in stats.txt'
    OPEN(UNIT=unit_num, FILE='stats.txt', STATUS='replace', ACTION='write')
    WRITE(unit_num, '(2A10)') 'AVG', 'STD'
    WRITE(unit_num, '(2F10.3)') avg, std
    CLOSE(unit_num)
END PROGRAM stats
!===========================================================
!   Module: Matrix Operator
!   Purpose: Basic functions relating to array and matrices
!===========================================================

MODULE matrix_operator
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

        ! Apply standard deviation formula
        std = ( SUM( (arr - average(arr)) ** 2 ) / n ) ** 0.5
    END FUNCTION standard_deviation

    REAL FUNCTION maximum(arr) RESULT(max)
        REAL, DIMENSION(:), INTENT(IN) :: arr
        INTEGER :: i ! Counter variable

        max = arr(1) ! First guess

        ! Iterate over each element to keep the biggest value
        DO i = 2, SIZE(arr)
            IF (arr(i) > max) max = arr(i)
        END DO
    END FUNCTION maximum

    ! Matrices operators

    REAL FUNCTION trace(mat) RESULT(tr)
        REAL, DIMENSION (:, :), INTENT(IN) :: mat
        INTEGER :: n, i ! Matrix size and indexing variable
        
        ! Check matrix (BONUS)
        CALL check_matrix(mat)

        ! Take size of the array
        n = SIZE(mat, 1)

        ! Compute trace
        tr = 0
        DO i = 1, n
            tr = tr + mat(i, i)
        END DO
    END FUNCTION trace

    REAL FUNCTION determinant(mat) RESULT(det)
        REAL, DIMENSION (:, :), INTENT(IN) :: mat
        INTEGER :: n, i ! Matrix size and indexing variable
        REAL :: m11, m12, m13, m21, m22, m23, m31, m32, m33 ! Matrix' element

        ! Check matrix (BONUS)
        CALL check_matrix(mat)

        ! Take size of the array
        n = SIZE(mat, 1)

        ! Check case by size
        IF (n == 2) THEN
            det = mat(1, 1) * mat(2, 2) - mat(1, 2) * mat(2, 1)
        ELSE ! n == 3, apply Sarrus' rule, column is the first dimension
        ! I think there must be a better way to write this easier
            m11 = mat(1, 1)
            m21 = mat(2, 1)
            m31 = mat(3, 1)
            m12 = mat(1, 2)
            m22 = mat(2, 2)
            m32 = mat(3, 2)
            m13 = mat(1, 3)
            m23 = mat(2, 3)
            m33 = mat(3, 3)
            det = m11*m22*m33 + m21*m32*m13 + m31*m12*m23 - m31*m22*m13 - m11*m32*m23 - m21*m12*m33
        END IF
    END FUNCTION determinant

    SUBROUTINE check_matrix (mat)
        REAL, DIMENSION(:, :), INTENT(IN) :: mat
        INTEGER :: n ! Matrix first dimension size

        ! BONUS: Ensure matrix is size is 2 or 3
        n = SIZE(mat, 1)
        IF ((n /= 2) .AND. (n /= 3)) STOP 'Inputted matrix has to be 2 x 2 or 3 x 3!'

        ! BONUS: Ensure matrix is square
        IF (n**2 /= SIZE(mat)) STOP 'Inputted matrix has to be square!'    
    END SUBROUTINE check_matrix
END MODULE matrix_operator

!===========================================================
!   Module: Read file
!   Purpose: Basic file reading
!===========================================================

MODULE read_file
    IMPLICIT NONE
CONTAINS
    SUBROUTINE read_flexible (file_name, mat, n_col, n_row)
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
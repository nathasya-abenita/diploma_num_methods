PROGRAM test
    USE matrix_operator
    IMPLICIT NONE
    INTEGER :: i, j
    REAL, DIMENSION(9) :: vec
    REAL, DIMENSION(3, 3) :: mat
    REAL, DIMENSION(3, 4) :: wrong_mat

    ! Define vector and 3x3 matrix directly
    vec = (/1, 2, 3, 4, 5, 6, 7, 8, 9/)
    mat = RESHAPE(vec, (/3, 3/)) ! reshape vec to 3 by 3 matrix

    ! Print elements
    DO i = 1, 3
        DO j = 1, 3
            PRINT*, i, j, mat(i, j)
        END DO
    END DO

    ! Print average and standard deviation
    PRINT*, 'Average:', average(vec)
    PRINT*, 'Standard deviation:', standard_deviation(vec)
    PRINT*, 'Maximum:', maximum(vec)

    ! Print trace and determinant
    PRINT*, 'Trace:', trace(mat)
    PRINT*, 'Determinant:', determinant(mat)

    ! Try to put wrong matrix in the function, expect an abrupt stop
    PRINT*, determinant(wrong_mat)
END PROGRAM test
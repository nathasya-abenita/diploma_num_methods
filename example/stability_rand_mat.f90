PROGRAM check_stability
    USE matrix_operator
    IMPLICIT NONE
    INTEGER :: n, i ! Number of trial, looping index
    REAL, DIMENSION(2, 2) :: a ! Coefficient matrix
    REAL, DIMENSION(5) :: counter ! Counter for the stability
    REAL :: tr, det, cr ! Trace, determinant, and criteria

    ! Initialize counter array with zeros
    counter = 0
    
    ! Define number of trial
    n = INT(1E6)

    ! Perform n times of experiment
    DO i = 1, n
        ! Generate random number with uniform distribution in [0, 1]
        CALL RANDOM_NUMBER(a)

        ! Resize the distribution to [-1, 1]
        a = -1 + 2 * a
        
        ! Compute parameters linked to stability criteria
        tr = trace(a)
        det = determinant(a)
        cr = tr ** 2 - 4 * det
        
        ! Classify the stability
        IF ((cr > 0) .AND. (tr < 0) .AND. (det > 0)) THEN
            counter(1) = counter(1) + 1
        ELSE IF ((cr > 0) .AND. (tr > 0) .AND. (det > 0)) THEN
            counter(2) = counter(2) + 1
        ELSE IF (det < 0) THEN
            counter(3) = counter(3) + 1
        ELSE IF ((cr < 0) .AND. (tr < 0)) THEN
            counter(4) = counter(4) + 1
        ELSE
            counter(5) = counter(5) + 1
        END IF
        
    END DO
    
    ! Print output
    PRINT*, 'Number of trial:', INT(SUM(counter))
    PRINT('(5A18)'), 'Stable Node', 'Unstable Node', 'Sadle Point', 'Stable Spiral', 'Unstable Spiral'
    PRINT('(5F18.3)'), counter * ( 1 / SUM(counter) )
END PROGRAM check_stability
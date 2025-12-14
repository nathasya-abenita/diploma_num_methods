PROGRAM fixed_seed_random
    IMPLICIT NONE
    INTEGER :: n, i
    INTEGER, ALLOCATABLE :: seed(:)
    REAL :: r

    ! Find out how many integers are needed for the seed
    CALL RANDOM_SEED(SIZE=n)
    ALLOCATE(seed(n))

    ! Set your own seed values (example: all 12345)
    seed = 12345
    CALL RANDOM_SEED(PUT=seed)

    ! Generate some random numbers
    DO i = 1, 5
    CALL RANDOM_NUMBER(r)
    PRINT *, r
    END DO
END PROGRAM fixed_seed_random
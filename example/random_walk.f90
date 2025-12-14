MODULE random_walk_module
    USE matrix_operator
    IMPLICIT NONE
CONTAINS
    SUBROUTINE random_walk (x)
        REAL, DIMENSION(:), INTENT(INOUT) :: x
        INTEGER :: i ! Looping index
        REAL :: r ! Probability for the walk

        ! Always set first position to be zero
        x(1) = 0

        ! Iterate over each step
        DO i = 2, SIZE(x)
            ! Call random number
            CALL RANDOM_NUMBER(r)

            ! Apply the walk
            IF (r < 0.5) THEN
                x(i) = x(i - 1) - 1
            ELSE
                x(i) = x(i - 1) + 1
            END IF
        END DO 
    END SUBROUTINE random_walk

    SUBROUTINE random_walks (x)
        REAL, DIMENSION(:, :), INTENT(INOUT) :: x
        REAL, DIMENSION(:), ALLOCATABLE :: walk_position
        INTEGER :: i ! Looping index

        ALLOCATE(walk_position(SIZE(x, 2)))

        DO i = 1, SIZE(x, 1)
            ! Compute positions for one random walk
            CALL random_walk(walk_position)
            ! Save it
            x(i, :) = walk_position
        END DO
    END SUBROUTINE random_walks

END MODULE random_walk_module

PROGRAM simulate_random_walk
    USE random_walk_module
    IMPLICIT NONE
    INTEGER :: n_trial, n_points, i ! Number of trials, looping index
    INTEGER :: n_step ! Step for random walk
    REAL, DIMENSION(:), ALLOCATABLE :: output_arr, gen_numbers
    INTEGER :: unit_num = 11 ! For writing files

    REAL, DIMENSION(:), ALLOCATABLE :: walk_position
    REAL, DIMENSION(:, :), ALLOCATABLE :: walk_positions

    ! Problem 1: Verify CLT

    ! Define number of trials and generated numbers
    n_trial = 100
    n_points = INT(1E4)

    ! Allocate list for generated numbers and output mean
    ALLOCATE(output_arr(n_trial), gen_numbers(n_points))
    
    ! Perform trials
    DO i = 1, n_trial
        ! Call numbers from U[0, 1]
        CALL RANDOM_NUMBER(gen_numbers)

        ! Compute average and save it
        output_arr(i) = average(gen_numbers)
    END DO

    ! Print output
    OPEN (UNIT=unit_num, FILE='./output/clt.txt', STATUS='replace', ACTION='write')
        WRITE(unit_num, *) output_arr
    CLOSE(unit_num)

    ! Problem 2: One-dimensional random walk, saving positions at all time
    n_trial = 10
    n_step = INT(1E3)
    ALLOCATE(walk_positions(n_trial, n_step), walk_position(n_step))

    ! Compute the positions for the random walks
    CALL random_walks(walk_positions)
    
    ! Prepare output
    OPEN (UNIT=unit_num, FILE='./output/position.txt', STATUS='replace', ACTION='write')
        DO i = 1, n_step
            WRITE(unit_num, *) walk_positions(:, i)
        END DO
    CLOSE(unit_num)

    ! Problem 3: One-dimensional random walk, saving last position
    n_trial = INT(1E5)
    n_step = INT(1E3)
    DEALLOCATE(walk_positions, walk_position)
    ALLOCATE(walk_positions(n_trial, n_step), walk_position(n_step))

    ! Compute the positions for the random walks
    CALL random_walks(walk_positions)
    
    ! Prepare output
    OPEN (UNIT=unit_num, FILE='./output/last_position.txt', STATUS='replace', ACTION='write')
        ! Write only the last position
        WRITE(unit_num, *) walk_positions(:, n_step)
    CLOSE(unit_num)

    ! Problem bonus: Save average positions and mean
    ! squared displacements

    ! Prepare output
    OPEN (UNIT=unit_num, FILE='./output/mean_positions.txt', STATUS='replace', ACTION='write')
        DO i = 1, n_step
            ! First column: average of position
            ! Second column: msd
            WRITE(unit_num, *) average(walk_positions(:, i)), &
            average(walk_positions(:, i) ** 2)
        END DO
    CLOSE(unit_num)

END PROGRAM simulate_random_walk
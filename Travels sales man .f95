

PROGRAM P4

implicit none
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Variable declarations
    
INTEGER :: status, count, i, j, distance, best_distance, permutations
CHARACTER(50) :: filename
INTEGER, DIMENSION(:,:), ALLOCATABLE :: dist_table      !2d int array of data
CHARACTER(10), DIMENSION(:), ALLOCATABLE :: cities      !char array for names of cities
INTEGER, DIMENSION(:), ALLOCATABLE :: path              !int array for temp path
INTEGER, DIMENSION(:), ALLOCATABLE :: best_path         !int array for best path
best_distance = 999999999       !sets best_distance to large number to compare with distance later
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Open the file and read number of cities
PRINT *, "Enter filename:"
READ *, filename
PRINT *, filename
OPEN(UNIT=15, FILE=filename, STATUS="OLD", ACTION="READ", IOSTAT=status)
IF(status /= 0) THEN
    PRINT *, "ERROR, could not open file for reading." 
    STOP
END IF       
READ (UNIT=15, FMT=100) count       !reads in number of cities
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Allocate memory for all needed arrays
    
ALLOCATE(dist_table(1:count, 1:count))
ALLOCATE(cities(1:count))
ALLOCATE(path(1:count))
ALLOCATE(best_path(1:count))
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Fill in arrays from data file
    
DO i = 1, count
    path(i) = i
    best_path(i) = i
    READ(UNIT=15, FMT=200) cities(i)    !reads in names for cities
    DO j = 1, count
        READ(UNIT=15, FMT=100) dist_table(i,j)  !reads in distances of cities
    END DO
END DO
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Use recursion to find minimal distance

call permute(2,count)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Print formatted output

PRINT *, " "
DO i = 1, count
    IF (i /= count) THEN
        PRINT *, cities(best_path(i))," to ", cities(best_path(i+1)), " -- ", dist_table(best_path(i),best_path(i+1)), " miles"
    ELSE
        PRINT *, cities(best_path(i))," to ", cities(best_path(1)), " -- ", dist_table(best_path(i),best_path(1)), " miles"
    END IF
END DO
PRINT *, " "
PRINT *, "Best distance is: ", best_distance, " miles"
                    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Format labels
100 FORMAT (I6)
200 FORMAT (A)
    
    
CONTAINS
! inner subroutine
RECURSIVE SUBROUTINE permute(first,last)
    IMPLICIT NONE
        INTEGER :: first, last, temp, i
        ! base case
        IF (first == last) THEN
            distance = dist_table(1,path(2))
            DO i = 2, (last - 1)
                distance = distance + dist_table(path(i),path(i+1))
            END DO      
            ! get distance from last city to back home
            distance = distance + dist_table(path(last),path(1))
    
            permutations = permutations + 1
                
            IF (distance < best_distance) THEN
                best_distance = distance
                DO i = 2, count
                    best_path(i) = path(i)
                END DO
            END IF
        ELSE
            ! mix it up
            DO i = first, last
                temp = path(first)
                path(first) = path(i)
                path(i) = temp
    
                ! recursion reduction step
                call permute(first+1,last)
    
                temp = path(first)
                path(first) = path(i)
                path(i) = temp
            END DO
        END IF
    RETURN
!Permute function
    
END SUBROUTINE permute        
    
END PROGRAM P4

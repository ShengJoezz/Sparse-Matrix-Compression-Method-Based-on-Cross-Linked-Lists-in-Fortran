MODULE constants
  INTEGER, PARAMETER :: MAXELEMS = 1000
  INTEGER, PARAMETER :: MAXELEM_PER_UNIT = 100
END MODULE constants

MODULE types
  USE constants
  IMPLICIT NONE
  TYPE element
    INTEGER :: row, col
    REAL :: value
  END TYPE element

TYPE unit_matrix
  INTEGER :: start_row, end_row, start_col, end_col
  INTEGER :: num_elems
  TYPE(element), ALLOCATABLE :: elems(:)
END TYPE unit_matrix

  TYPE node
    INTEGER :: row, col
    REAL :: value
    TYPE(node), POINTER :: right, down
  END TYPE node

  CONTAINS

    LOGICAL FUNCTION is_position_equal(node1, node2)
      TYPE(node), POINTER, INTENT(IN) :: node1, node2
      is_position_equal = (node1%row == node2%row .AND. node1%col == node2%col)
    END FUNCTION is_position_equal

END MODULE types

PROGRAM assemble_global_matrix
  USE constants
  USE types
  IMPLICIT NONE

  TYPE(unit_matrix), ALLOCATABLE :: units(:)
  TYPE(node), POINTER :: head
  INTEGER :: num_units, unit_id, i, j, global_size, iostat

  ! Read the number of unit matrices
  PRINT *, "Enter the number of unit matrices: "
  READ *, num_units
  IF (num_units <= 0) THEN
    PRINT *, "Error: Number of unit matrices must be positive."
    STOP
  END IF
  ALLOCATE(units(num_units))

  ! Read the unit matrices
DO i = 1, num_units
  PRINT *, "Enter the start row, end row, start column, and end column of unit matrix ", i, ": "
  READ *, units(i)%start_row, units(i)%end_row, units(i)%start_col, units(i)%end_col
  PRINT *, "Enter the number of non-zero elements in unit matrix ", i, ": "
  READ *, units(i)%num_elems
  IF (units(i)%num_elems > MAXELEM_PER_UNIT) THEN
    PRINT *, "Error: Number of elements exceeds the maximum allowed."
    STOP
  END IF
  ALLOCATE(units(i)%elems(units(i)%num_elems))
  PRINT *, "Enter the row, column in the global matrix, and value of each non-zero element: "
  DO j = 1, units(i)%num_elems
    READ(*, *, IOSTAT=iostat) units(i)%elems(j)%row, units(i)%elems(j)%col, units(i)%elems(j)%value
    IF (iostat /= 0) THEN
      PRINT *, "Error: Invalid input format for non-zero element."
      STOP
    END IF
    IF (units(i)%elems(j)%row < units(i)%start_row .OR. units(i)%elems(j)%row > units(i)%end_row .OR. &
        units(i)%elems(j)%col < units(i)%start_col .OR. units(i)%elems(j)%col > units(i)%end_col) THEN
      PRINT *, "Error: Row and column indices must be within the unit matrix range."
      STOP
    END IF
  END DO
END DO

  ! Read the size of the global matrix
  PRINT *, "Enter the size of the global matrix: "
  READ *, global_size
  IF (global_size <= 0) THEN
    PRINT *, "Error: Size of the global matrix must be positive."
    STOP
  END IF

  NULLIFY(head)
  CALL assemble_matrix(head, global_size, units, num_units)
  CALL print_matrix(head, global_size)
  CALL free_linked_list(head)

  DO i = 1, num_units
    DEALLOCATE(units(i)%elems)
  END DO
  DEALLOCATE(units)

CONTAINS

SUBROUTINE assemble_matrix(head, size, units, num_units)
  TYPE(node), POINTER, INTENT(INOUT) :: head
  INTEGER, INTENT(IN) :: size, num_units
  TYPE(unit_matrix), INTENT(IN) :: units(:)
  TYPE(node), POINTER :: curr, new_node, row_ptr, col_ptr, prev_row, prev_col
  INTEGER :: i, j

  NULLIFY(head)

  DO i = 1, num_units
    DO j = 1, units(i)%num_elems
      IF (units(i)%elems(j)%value == 0.0) THEN
        CYCLE
      END IF

      ! Search for the element in the existing linked list
      row_ptr => head
      col_ptr => NULL()
      DO WHILE (ASSOCIATED(row_ptr))
        IF (row_ptr%row == units(i)%elems(j)%row) THEN
          col_ptr => row_ptr%down
          DO WHILE (ASSOCIATED(col_ptr))
            IF (col_ptr%col == units(i)%elems(j)%col) THEN
              ! Accumulate the value if element already exists
              col_ptr%value = col_ptr%value + units(i)%elems(j)%value
              EXIT
            ELSE IF (col_ptr%col > units(i)%elems(j)%col) THEN
              EXIT
            END IF
            col_ptr => col_ptr%down
          END DO
          IF (ASSOCIATED(col_ptr)) THEN
            EXIT
          END IF
        ELSE IF (row_ptr%row > units(i)%elems(j)%row) THEN
          EXIT
        END IF
        row_ptr => row_ptr%right
      END DO

      IF (.NOT. ASSOCIATED(col_ptr)) THEN
        ! Element not found, insert a new node
        ALLOCATE(new_node)
        new_node%row = units(i)%elems(j)%row
        new_node%col = units(i)%elems(j)%col
        new_node%value = units(i)%elems(j)%value
        NULLIFY(new_node%right)
        NULLIFY(new_node%down)

        ! Insert into the row list
        IF (ASSOCIATED(row_ptr)) THEN
          prev_row => row_ptr
          curr => row_ptr%right
          DO WHILE (ASSOCIATED(curr))
            IF (curr%row >= new_node%row) THEN
              EXIT
            END IF
            prev_row => curr
            curr => curr%right
          END DO
          new_node%right => prev_row%right
          prev_row%right => new_node
        ELSE
          head => new_node
        END IF

        ! Insert into the column list
        IF (ASSOCIATED(row_ptr)) THEN
          IF (row_ptr%row == new_node%row) THEN
            prev_col => row_ptr
            col_ptr => row_ptr%down
            DO WHILE (ASSOCIATED(col_ptr))
              IF (col_ptr%col >= new_node%col) THEN
                EXIT
              END IF
              prev_col => col_ptr
              col_ptr => col_ptr%down
            END DO
            new_node%down => prev_col%down
            prev_col%down => new_node
          ELSE
            new_node%down => row_ptr%down
            row_ptr%down => new_node
          END IF
        END IF
      END IF
    END DO
  END DO
END SUBROUTINE assemble_matrix

SUBROUTINE print_matrix(head, size)
  TYPE(node), POINTER, INTENT(IN) :: head
  INTEGER, INTENT(IN) :: size
  TYPE(node), POINTER :: row_ptr, col_ptr
  REAL, ALLOCATABLE :: row_values(:)
  INTEGER :: i, j

  ALLOCATE(row_values(size))

  DO i = 1, size
    row_ptr => head
    DO WHILE (ASSOCIATED(row_ptr) .AND. row_ptr%row < i)
      row_ptr => row_ptr%right
    END DO

    IF (ASSOCIATED(row_ptr) .AND. row_ptr%row == i) THEN
      row_values = 0.0
      col_ptr => row_ptr
      DO WHILE (ASSOCIATED(col_ptr))
        row_values(col_ptr%col) = col_ptr%value
        col_ptr => col_ptr%down
      END DO
    ELSE
      row_values = 0.0
    END IF

    WRITE(*, '(*(F10.5, 1X))') (row_values(j), j=1,size)
  END DO

  DEALLOCATE(row_values)
END SUBROUTINE print_matrix

  RECURSIVE SUBROUTINE free_linked_list(head)
  TYPE(node), POINTER :: head, curr, next_row, next_col

  IF (ASSOCIATED(head)) THEN
    curr => head
    DO WHILE (ASSOCIATED(curr))
      next_row => curr%right
      IF (ASSOCIATED(curr%down)) THEN
        CALL free_linked_list(curr%down)
      END IF
      DEALLOCATE(curr)
      curr => next_row
    END DO
  END IF
END SUBROUTINE free_linked_list

END PROGRAM assemble_global_matrix
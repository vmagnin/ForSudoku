! file:  check.f90
! date:  nbehrnd [2023-08-24 Thu]
! edit:  vmagnin [2024-03-16 Fri]

! This file contains tests to be launched by `fpm test`.

program check
  use sudoku, only: Read_grid, Solve_grid, ValidColumOrRow, ValidZone, &
                  & ValidGrid, ValidDigit

  implicit none
  integer, dimension(9, 9) :: full_grid, ref_grid

  ! Wikipedia's complete Sudoku grid:
  !&<
  full_grid(:, 1) = [5, 3, 4,  6, 7, 8,  9, 1, 2]
  full_grid(:, 2) = [6, 7, 2,  1, 9, 5,  3, 4, 8]
  full_grid(:, 3) = [1, 9, 8,  3, 4, 2,  5, 6, 7]

  full_grid(:, 4) = [8, 5, 9,  7, 6, 1,  4, 2, 3]
  full_grid(:, 5) = [4, 2, 6,  8, 5, 3,  7, 9, 1]
  full_grid(:, 6) = [7, 1, 3,  9, 2, 4,  8, 5, 6]

  full_grid(:, 7) = [9, 6, 1,  5, 3, 7,  2, 8, 4]
  full_grid(:, 8) = [2, 8, 7,  4, 1, 9,  6, 3, 5]
  full_grid(:, 9) = [3, 4, 5,  2, 8, 6,  1, 7, 9]
  !&>

  call unit_tests()

  ! An incomplete Sudoku grid with implicitly empty cases:
  !&<
  ref_grid(:, 1) = [5, 3, 0,  0, 7, 0,  0, 0, 0]
  ref_grid(:, 2) = [6, 0, 0,  1, 9, 5,  0, 0, 0]
  ref_grid(:, 3) = [0, 9, 8,  0, 0, 0,  0, 6, 0]

  ref_grid(:, 4) = [8, 0, 0,  0, 6, 0,  0, 0, 3]
  ref_grid(:, 5) = [4, 0, 0,  8, 0, 3,  0, 0, 1]
  ref_grid(:, 6) = [7, 0, 0,  0, 2, 0,  0, 0, 6]

  ref_grid(:, 7) = [0, 6, 0,  0, 0, 0,  2, 8, 0]
  ref_grid(:, 8) = [0, 0, 0,  4, 1, 9,  0, 0, 5]
  ref_grid(:, 9) = [0, 0, 0,  0, 8, 0,  0, 7, 9]
  !&>

  call assert_readtest01()
  call assert_readtest02()
  call assert_wikipedia_solution()

contains

  subroutine unit_tests
    integer, dimension(9, 9)     :: grid
    integer, dimension(1:3, 1:3) :: region
    integer :: i, j

    if (.not.ValidColumOrRow([1,2,3,4,5,6,7,8,9])) error stop "ValidColumOrRow() not working!"
    if (ValidColumOrRow([1,2,3,4,5,6,7,8,1])) error stop "ValidColumOrRow() not working!"
    if (ValidColumOrRow([4,2,3,4,9,8,7,6,5])) error stop "ValidColumOrRow() not working!"

    region = reshape([ &
       1, 2, 3, &
       4, 5, 6, &
       7, 8, 9  &
      ], shape(region), order=[2,1])
    if (.not.ValidZone(region)) error stop "ValidZone() not working! (1)"

    region(2, 2) = 2
    if (ValidZone(region)) error stop "ValidZone() not working! (2)"
    region(1, 1) = 9
    if (ValidZone(region)) error stop "ValidZone() not working! (3)"

    grid = full_grid
    if (.not.ValidGrid(grid)) error stop "ValidGrid() not working! (1)"
    grid(2, 2) = 8
    if (ValidGrid(grid)) error stop "ValidGrid() not working! (2)"

    grid = full_grid
    do i = 1, 9
      do j = 1, 9
        if (.not.ValidDigit(grid, i, j)) error stop "ValidDigit() not working! (1)"
      end do
    end do
    grid(4, 4) = 1
    if (ValidDigit(grid, 4, 4)) error stop "ValidDigit() not working! (2)"
  end subroutine unit_tests

  subroutine assert_readtest01()
    ! read an incomplete Sudoku grid with implicitly empty cases
    integer :: reference_grid(9, 9), grid_from_file(9, 9)
    integer :: i, j
    logical :: array_equality

    array_equality = .true.

    reference_grid = ref_grid

    call Read_grid(grid_from_file, "./test/test_in_01.txt")
    grid_from_file = transpose(grid_from_file)

    outer: do i = 1, 9
      ! write (*, "(9I3)") grid_from_file(:,i)
      inner: do j = 1, 9
        if (reference_grid(i, j) /= grid_from_file(i, j)) then
          array_equality = .false.
          print *, "At i : ", i, "j : ", j, &
            "reference_grid : ", reference_grid(i, j), &
            "differs from grid_from_file :", grid_from_file(i, j)
          exit outer
        end if
      end do inner
    end do outer

    if (array_equality .eqv. .false.) then
      print *, "Reading check on `test_in_01.txt` failed."
    else
      print *, "Reading check on `test_in_01.txt` was successful."
    end if
  end subroutine assert_readtest01

  subroutine assert_readtest02()
    ! read an incomplete Sudoku grid with explicitly empty cases
    integer :: reference_grid(9, 9), grid_from_file(9, 9)
    integer :: i, j
    logical :: array_equality

    array_equality = .true.

    reference_grid = ref_grid

    call Read_grid(grid_from_file, "./test/test_in_02.txt")
    grid_from_file = transpose(grid_from_file)

    outer: do i = 1, 9
      ! write (*, "(9I3)") grid_from_file(:,i)
      inner: do j = 1, 9
        if (reference_grid(i, j) /= grid_from_file(i, j)) then
          array_equality = .false.
          print *, "At i : ", i, "j : ", j, &
            "reference_grid : ", reference_grid(i, j), &
            "differs from grid_from_file :", grid_from_file(i, j)
          exit outer
        end if
      end do inner
    end do outer

    if (array_equality .eqv. .false.) then
      print *, "Reading check on `test_in_02.txt` failed."
    else
      print *, "Reading check on `test_in_02.txt` was successful."
    end if
  end subroutine assert_readtest02

  subroutine assert_wikipedia_solution()
    ! see the reference grids on https://en.wikipedia.org/wiki/Sudoku
    ! local variables:
    integer :: grid_a(9, 9), grid_b(9, 9)
    integer :: i, j
    logical :: array_equality

    array_equality = .true.

    ! Wikipedia's incomplete Sudoku grid
    grid_a = ref_grid

    call Solve_grid(grid_a) ! this fills (hence modifies) grid_a

    ! Wikipedia's complete Sudoku grid
    grid_b = full_grid

    ! comparison of computed solution with Wikipedia's reference solution
    outer: do i = 1, 9
      do j = 1, 9
        if (grid_a(i, j) /= grid_b(i, j)) then
          array_equality = .false.
          print *, "At i : ", i, "j : ", j, "grid_a : ", grid_a(i, j), &
            "the solution differs from expected value of grid_b :", grid_b(i, j)
          exit outer
        end if
      end do
    end do outer

    if (array_equality .eqv. .false.) then
      print *, "The Wikipedia array assertion failed."
    else
      print *, "The Wikipedia array assertion was successful."
    end if
  end subroutine assert_wikipedia_solution
end program check

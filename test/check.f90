! file:  check.f90
! date:  nbehrnd [2023-08-24 Thu]
! edit:  vmagnin [2024-03-18 Fri]

! This file contains tests to be launched by `fpm test`.

program check
  use sudoku, only: read_grid, solve_puzzle, valid_colum_or_row, valid_zone, &
                  & valid_grid, valid_digit, list_possible_digits, is_full, &
                  & create_puzzle_with_unique_solution

  implicit none
  integer, dimension(9, 9) :: full_ref_grid, ref_puzzle

  ! Wikipedia's complete Sudoku grid:
  !&<
  full_ref_grid(:, 1) = [5, 3, 4,  6, 7, 8,  9, 1, 2]
  full_ref_grid(:, 2) = [6, 7, 2,  1, 9, 5,  3, 4, 8]
  full_ref_grid(:, 3) = [1, 9, 8,  3, 4, 2,  5, 6, 7]

  full_ref_grid(:, 4) = [8, 5, 9,  7, 6, 1,  4, 2, 3]
  full_ref_grid(:, 5) = [4, 2, 6,  8, 5, 3,  7, 9, 1]
  full_ref_grid(:, 6) = [7, 1, 3,  9, 2, 4,  8, 5, 6]

  full_ref_grid(:, 7) = [9, 6, 1,  5, 3, 7,  2, 8, 4]
  full_ref_grid(:, 8) = [2, 8, 7,  4, 1, 9,  6, 3, 5]
  full_ref_grid(:, 9) = [3, 4, 5,  2, 8, 6,  1, 7, 9]
  !&>

  ! An incomplete Sudoku grid with implicitly empty cases:
  !&<
  ref_puzzle(:, 1) = [5, 3, 0,  0, 7, 0,  0, 0, 0]
  ref_puzzle(:, 2) = [6, 0, 0,  1, 9, 5,  0, 0, 0]
  ref_puzzle(:, 3) = [0, 9, 8,  0, 0, 0,  0, 6, 0]

  ref_puzzle(:, 4) = [8, 0, 0,  0, 6, 0,  0, 0, 3]
  ref_puzzle(:, 5) = [4, 0, 0,  8, 0, 3,  0, 0, 1]
  ref_puzzle(:, 6) = [7, 0, 0,  0, 2, 0,  0, 0, 6]

  ref_puzzle(:, 7) = [0, 6, 0,  0, 0, 0,  2, 8, 0]
  ref_puzzle(:, 8) = [0, 0, 0,  4, 1, 9,  0, 0, 5]
  ref_puzzle(:, 9) = [0, 0, 0,  0, 8, 0,  0, 7, 9]
  !&>

  call unit_tests()
  call assert_readtest01()
  call assert_readtest02()
  call assert_wikipedia_solution()

contains

  subroutine unit_tests
    integer, dimension(9, 9)     :: grid
    integer, dimension(1:3, 1:3) :: region
    integer :: i, j, empty

    if (.not.valid_colum_or_row([1,2,3,4,5,6,7,8,9])) error stop "valid_colum_or_row() not working!"
    if (valid_colum_or_row([1,2,3,4,5,6,7,8,1])) error stop "valid_colum_or_row() not working!"
    if (valid_colum_or_row([4,2,3,4,9,8,7,6,5])) error stop "valid_colum_or_row() not working!"

    region = reshape([ &
       1, 2, 3, &
       4, 5, 6, &
       7, 8, 9  &
      ], shape(region), order=[2,1])
    if (.not.valid_zone(region)) error stop "valid_zone() not working! (1)"

    region(2, 2) = 2
    if (valid_zone(region)) error stop "valid_zone() not working! (2)"
    region(1, 1) = 9
    if (valid_zone(region)) error stop "valid_zone() not working! (3)"

    grid = full_ref_grid
    if (.not.valid_grid(grid)) error stop "valid_grid() not working! (1)"
    grid(2, 2) = 8
    if (valid_grid(grid)) error stop "valid_grid() not working! (2)"

    grid = full_ref_grid
    do i = 1, 9
      do j = 1, 9
        if (.not.valid_digit(grid, i, j)) error stop "valid_digit() not working! (1)"
      end do
    end do
    grid(4, 4) = 1
    if (valid_digit(grid, 4, 4)) error stop "valid_digit() not working! (2)"

    grid = full_ref_grid
    if (.not.is_full(grid)) error stop "is_full() not working! (1)"
    grid = ref_puzzle
    if (is_full(grid)) error stop "is_full() not working! (2)"

    block
      integer :: nb_possible
      integer, dimension(1:9) :: possible_digit
      call list_possible_digits(ref_puzzle, 8, 2, nb_possible, possible_digit)
      if (nb_possible /= 3) error stop "list_possible_digits() not working! (1)"
      if (.not. all(possible_digit == [2, 3, 4, 0, 0, 0, 0, 0, 0])) error stop "list_possible_digits() not working! (2)"
    end block

    grid = full_ref_grid
    call create_puzzle_with_unique_solution(grid, empty)
    call solve_puzzle(grid)   ! This fills grid
    if (any(grid /= full_ref_grid)) error stop "create_puzzle_with_unique_solution() not working! (1)"
    if ((empty == 0).or.(empty > 81)) error stop "create_puzzle_with_unique_solution() not working! (2)"
  end subroutine unit_tests

  subroutine assert_readtest01()
    ! Read an incomplete Sudoku grid with implicitly empty cases
    integer :: reference_grid(9, 9), grid_from_file(9, 9)
    integer :: i, j
    logical :: array_equality

    array_equality = .true.

    reference_grid = ref_puzzle

    call read_grid(grid_from_file, "./test/test_in_01.txt")
    grid_from_file = transpose(grid_from_file)

    outer: do i = 1, 9
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
    ! Read an incomplete Sudoku grid with explicitly empty cases
    integer :: reference_grid(9, 9), grid_from_file(9, 9)
    integer :: i, j
    logical :: array_equality

    array_equality = .true.

    reference_grid = ref_puzzle

    call read_grid(grid_from_file, "./test/test_in_02.txt")
    grid_from_file = transpose(grid_from_file)

    outer: do i = 1, 9
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
    ! See the reference grids on https://en.wikipedia.org/wiki/Sudoku
    integer :: grid_a(9, 9), grid_b(9, 9)
    integer :: i, j
    logical :: array_equality

    array_equality = .true.

    ! Wikipedia's incomplete Sudoku grid:
    grid_a = ref_puzzle

    call solve_puzzle(grid_a) ! This fills (hence modifies) grid_a

    ! Wikipedia's complete Sudoku grid
    grid_b = full_ref_grid

    ! Comparison of computed solution with Wikipedia's reference solution:
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

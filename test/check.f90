! file:  check.f90
! date:  [2023-08-24 Thu]
! edit:  [2023-09-05 Tue]

! This file contains tests to be launched by `fpm test`.

program check
   use sudoku, only: Read_grid, Solve_grid
   implicit none

   call assert_readtest01()
   call assert_readtest02()
   call assert_wikipedia_solution()

contains

   subroutine assert_readtest01()
   ! read an incomplete Sudoku grid with implicitly empty cases
   integer :: reference_grid(9,9), grid_from_file(9,9)
   integer :: i, j
   logical :: array_equality

   array_equality = .true.

      reference_grid(:, 1) = [5, 3, 0,  0, 7, 0,  0, 0, 0]
      reference_grid(:, 2) = [6, 0, 0,  1, 9, 5,  0, 0, 0]
      reference_grid(:, 3) = [0, 9, 8,  0, 0, 0,  0, 6, 0]

      reference_grid(:, 4) = [8, 0, 0,  0, 6, 0,  0, 0, 3]
      reference_grid(:, 5) = [4, 0, 0,  8, 0, 3,  0, 0, 1]
      reference_grid(:, 6) = [7, 0, 0,  0, 2, 0,  0, 0, 6]

      reference_grid(:, 7) = [0, 6, 0,  0, 0, 0,  2, 8, 0]
      reference_grid(:, 8) = [0, 0, 0,  4, 1, 9,  0, 0, 5]
      reference_grid(:, 9) = [0, 0, 0,  0, 8, 0,  0, 7, 9]

      call Read_grid(grid_from_file, "./test/test_in_01.txt")
      grid_from_file = transpose(grid_from_file)

      outer: do i = 1, 9
         ! write (*, "(9I3)") grid_from_file(:,i)
         inner: do j = 1, 9
            if (reference_grid(i, j) /= grid_from_file(i, j)) then
               array_equality = .false.
               print *, "At i : ", i, "j : ", j, &
                  "reference_grid : ", reference_grid(i,j), &
                  "differs from grid_from_file :", grid_from_file(i,j)
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
   integer :: reference_grid(9,9), grid_from_file(9,9)
   integer :: i, j
   logical :: array_equality

   array_equality = .true.

      reference_grid(:, 1) = [5, 3, 0,  0, 7, 0,  0, 0, 0]
      reference_grid(:, 2) = [6, 0, 0,  1, 9, 5,  0, 0, 0]
      reference_grid(:, 3) = [0, 9, 8,  0, 0, 0,  0, 6, 0]

      reference_grid(:, 4) = [8, 0, 0,  0, 6, 0,  0, 0, 3]
      reference_grid(:, 5) = [4, 0, 0,  8, 0, 3,  0, 0, 1]
      reference_grid(:, 6) = [7, 0, 0,  0, 2, 0,  0, 0, 6]

      reference_grid(:, 7) = [0, 6, 0,  0, 0, 0,  2, 8, 0]
      reference_grid(:, 8) = [0, 0, 0,  4, 1, 9,  0, 0, 5]
      reference_grid(:, 9) = [0, 0, 0,  0, 8, 0,  0, 7, 9]

      call Read_grid(grid_from_file, "./test/test_in_02.txt")
      grid_from_file = transpose(grid_from_file)

      outer: do i = 1, 9
         ! write (*, "(9I3)") grid_from_file(:,i)
         inner: do j = 1, 9
            if (reference_grid(i, j) /= grid_from_file(i, j)) then
               array_equality = .false.
               print *, "At i : ", i, "j : ", j, &
                  "reference_grid : ", reference_grid(i,j), &
                  "differs from grid_from_file :", grid_from_file(i,j)
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
      integer :: grid_a(9,9), grid_b(9,9)
      integer :: i, j
      logical :: array_equality

      array_equality = .true.

      ! Wikipedia's incomplete Sudoku grid
      grid_a(:, 1) = [5, 3, 0,  0, 7, 0,  0, 0, 0]
      grid_a(:, 2) = [6, 0, 0,  1, 9, 5,  0, 0, 0]
      grid_a(:, 3) = [0, 9, 8,  0, 0, 0,  0, 6, 0]

      grid_a(:, 4) = [8, 0, 0,  0, 6, 0,  0, 0, 3]
      grid_a(:, 5) = [4, 0, 0,  8, 0, 3,  0, 0, 1]
      grid_a(:, 6) = [7, 0, 0,  0, 2, 0,  0, 0, 6]

      grid_a(:, 7) = [0, 6, 0,  0, 0, 0,  2, 8, 0]
      grid_a(:, 8) = [0, 0, 0,  4, 1, 9,  0, 0, 5]
      grid_a(:, 9) = [0, 0, 0,  0, 8, 0,  0, 7, 9]

      call Solve_grid(grid_a) ! this fills (hence modifies) grid_a

      ! Wikipedia's complete Sudoku grid
      grid_b(:, 1) = [5, 3, 4,  6, 7, 8,  9, 1, 2]
      grid_b(:, 2) = [6, 7, 2,  1, 9, 5,  3, 4, 8]
      grid_b(:, 3) = [1, 9, 8,  3, 4, 2,  5, 6, 7]

      grid_b(:, 4) = [8, 5, 9,  7, 6, 1,  4, 2, 3]
      grid_b(:, 5) = [4, 2, 6,  8, 5, 3,  7, 9, 1]
      grid_b(:, 6) = [7, 1, 3,  9, 2, 4,  8, 5, 6]

      grid_b(:, 7) = [9, 6, 1,  5, 3, 7,  2, 8, 4]
      grid_b(:, 8) = [2, 8, 7,  4, 1, 9,  6, 3, 5]
      grid_b(:, 9) = [3, 4, 5,  2, 8, 6,  1, 7, 9]

      ! comparison of computed solution with Wikipedia's reference solution
      outer: do i = 1, 9
         do j = 1, 9
            if (grid_a(i, j) /= grid_b(i, j)) then
               array_equality = .false.
               print *, "At i : ", i, "j : ", j, "grid_a : ", grid_a(i,j), &
                  "the solution differs from expected value of grid_b :", grid_b(i,j)
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

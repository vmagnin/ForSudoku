! This file is part of the ForSudoku Fortran project.
! Copyright (C) 2006-2024 Vincent Magnin & Norwid Behrnd
!
! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by Vincent Magnin, 2006-11-27; Norwid Behrnd, 2023
! Last modifications: 2024-03-20
!------------------------------------------------------------------------------

program main
  use sudoku

  implicit none
  integer, dimension(9, 9) :: grid, grid0
  real          :: start, finish  ! To monitor the duration of the computation
  integer       :: choice
  integer       :: remainder      ! Number of filled cells when creating a grid
  integer       :: empty
  character(50) :: file           ! File name (including extension .txt)

  select case (command_argument_count())
  case (0)    ! The typical invocation with `fpm run`
    ! Pass 0 as a seed if you always need the same pseudo-random sequence
    ! for debugging or testing:
    call initialize_random_number_generator()

    ! Initialize an explicitly empty grid (empty cells are represented by 0):
    grid = 0

    print *, "ForSudoku 0.9.0, copyright (c) 2006-2024 Vincent Magnin and Norwid Behrnd"

    do
      print *
      print *, "******************************** MENU **************************************"
      print *, "1) Manual input (lines of comma separated 1 - 9, or 0 (empty cell))."
      print *, "2) Read a grid from a text file (for permitted patterns, see the doc)."
      print *, "3) Save the current grid in a text file."
      print *, "4) Check the validity of the current grid."
      print *, "5) Display the current grid."
      print *, "6) Create a random full Sudoku grid."
      print *, "7) Solve the puzzle grid currently stored in memory."
      print *, "8) Create a puzzle grid with a unique solution, starting from the grid in memory."
      print *, "9) Create a puzzle grid with a unique solution with n given digits."
      print *, "10) Create a puzzle grid (unicity not guaranteed)."
      print *, "0) Quit."
      write(*, '(A)', advance='no') "Type your choice and 'Enter': "
      read *, choice
      print *

      select case (choice)
      case (1)
        call request_grid(grid)
        print *, "You entered the following grid:"
        call display_grid(grid)
        call print_validity(grid, "The Sudoku is valid.", "The Sudoku is invalid.")

      case (2)
        print *, "Enter the file name of the grid to read (including .txt extension):"
        call execute_command_line("dir *.txt")
        read *, file
        call read_grid(grid, trim(file))
        call display_grid(grid)
        call print_validity(grid, "The Sudoku is valid.", "The Sudoku is invalid.")

      case (3)
        print *, "Enter the file name (including .txt) for saving the grid:"
        read *, file
        call save_grid(grid, trim(file))
        print *, "File saved."

      case (4)
        call print_validity(grid, "The grid is valid.", "The grid to is invalid.")

      case (5)
        print *, "The current grid in memory is:"
        call display_grid(grid)
        call print_validity(grid, "The grid is valid.", "The grid is invalid!")

      case (6)
        call cpu_time(start)
        call create_filled_grid(grid)
        call display_grid(grid)
        ! Useless test, unless there is a bug!
        call print_validity(grid, "The grid is valid.", "Computational error:  the grid is invalid!")
        call cpu_time(finish)
        write (*, "(A, F12.3, A)") " Computing time: ", finish - start, " s."

      case (7)
        print *, "The puzzle to solve is:"
        call display_grid(grid)
        call cpu_time(start)
        call solve_puzzle(grid)
        call print_validity(grid, "The solved grid (validity was verified):", &
                                & "Impossible to solve... (the puzzle was invalid)")
        call display_grid(grid)
        call cpu_time(finish)
        write (*, "(A, F12.3, A)") " Computing time: ", finish - start, " s."

      case (8)
        print *, "If a full valid grid is not in memory, we will start from a new one."
        print *, "The number of given digits is a priori unknown."

        if ((.not.is_full(grid)).or.(.not.valid_grid(grid))) then
          print *, "No full valid grid is in memory:  a new one will be generated."
          call create_filled_grid(grid)
        end if

        print *, "The starting full sudoku grid:"
        call display_grid(grid)
        ! grid validation (useless, but by security):
        call print_validity(grid, "The grid is valid.", "The grid is invalid: problem to compute a solution!")

        call cpu_time(start)
        call create_puzzle_with_unique_solution(grid, empty)
        print *, "A Sudoku puzzle with a unique solution:"
        call display_grid(grid)
        print *, "Empty cells: ", empty
        call print_validity(grid, "Valid grid", "Invalid grid: problem to compute a solution!")
        call cpu_time(finish)
        write (*, "(A, F12.3, A)") " Computing time: ", finish - start, " s."

      case (9)
        print *, "How many given digits in [17, 81] do you want? (below 33 it will become longer)"
        read *, remainder
        if (remainder < 33) print *, "Be patient..."

        call cpu_time(start)
        call create_filled_grid(grid0)

        ! We compute until the target is attained:
        do
          grid = grid0
          ! grid has intent(inout):
          call create_puzzle_with_unique_solution(grid, empty)
          if (81 - empty == remainder) exit
        end do

        print *, "This is a Sudoku puzzle with a unique solution:"
        call display_grid(grid)
        print '(" Empty cells: ", I0, "    Filled cells: ", I0)', empty,  81-empty
        call print_validity(grid, "Valid grid", "Invalid grid: problem to compute a solution!")
        call cpu_time(finish)
        write (*, "(A, F12.3, A)") " Computing time: ", finish - start, " s."

      case (10)
        print *, "If a full valid grid is not in memory, we will start from a new one."
        print *, "How many given digits in [17, 81] do you want in the puzzle grid?"
        print *, "(Below 28, the computation will become longer!)"
        read *, remainder

        if ((.not.is_full(grid)).or.(.not.valid_grid(grid))) then
          print *, "No full valid grid is in memory:  a new one will be generated."
          call create_filled_grid(grid)
        end if

        print *, "The starting full grid:"
        call display_grid(grid)
        ! grid validation:
        call print_validity(grid, "The grid is valid.", "The grid is invalid: problem to compute a solution!")

        call cpu_time(start)
        call create_puzzle(grid, remainder)
        print *, "A Sudoku puzzle (but not sure that the solution is unique):"
        call display_grid(grid)
        call print_validity(grid, "Valid grid", "Invalid grid: problem to compute a solution!")
        call cpu_time(finish)
        write (*, "(A, F12.3, A)") " Computing time: ", finish - start, " s."

      case (0)
        stop
      end select
    end do

  case (1) ! accessible only by direct invocation of the executable
    call get_command_argument(1, file)
    call solver(grid, file)

  case default
    print *, "Parameters: enter either one, or none."

  end select

contains

  ! Prints a message depending on the validity of the grid:
  subroutine print_validity(grid, yes, no)
    integer, dimension(9, 9), intent(in) :: grid
    character(*), intent(in) :: yes, no

    if (valid_grid(grid)) then
      print *, yes
    else
      print *, no
    end if
  end subroutine
end program main

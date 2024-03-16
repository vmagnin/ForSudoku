! This file is part of the sudoku Fortran project.
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
! Last modifications: 2024-03-16
!------------------------------------------------------------------------------

program main
  use sudoku

  implicit none
  integer, dimension(9, 9) :: grid
  real          :: Start, End  ! monitor the duration of computation
  integer       :: choice
  integer       :: remainder   ! number of filled cells when creating a grid
  character(50) :: file        ! file name (including extension .txt)

  select case (command_argument_count())
  case (0) ! the typical invocation with `fpm run`
    ! initialize the pseudorandom number generator
    call Initialize_Random
    ! initialize an explicitly empty grid
    grid = 0

    print *, "sudoku.f90, version 0.8.2, copyright (C) 2006-2024 Vincent Magnin and Norwid Behrnd"
    ! provide a user menu
    do
      print *
      print *, "*************************** MENU *****************************************"
      print *, "1) Manual input (lines of comma separated 1 - 9, or 0 (empty cell))."
      print *, "2) Input from a text file.  For permitted patterns, see the documentation."
      print *, "3) Save the currently processed grid as a text file."
      print *, "4) Check the validity of the grid currently stored in memory."
      print *, "5) Display the grid currently stored in memory."
      print *, "6) Create a random, already filled Sudoku grid."
      print *, "7) Solve the Sudoku grid currently stored in memory."
      print *, "8) Create a puzzle grid (with some probability that the solution is unique)."
      print *, "9) Quit."
      print *, "Select one of them and type `Enter`:"
      read *, choice
      print *

      select case (choice)
      case (1)
        call Request_grid(grid)
        print *, "You entered the following Sudoku:"
        call Display_grid(grid)
        call print_validity(grid, "The Sudoku is valid.", "The Sudoku is invalid.")

      case (2)
        print *, "Enter complete file name of the file to read (including .txt extension):"
        call execute_command_line("dir *.txt")
        read *, file
        call Read_grid(grid, trim(file))
        call Display_grid(grid)
        call print_validity(grid, "The Sudoku is valid.", "The Sudoku is invalid.")

      case (3)
        print *, "Enter complete file name of the file to save (incl. .txt):"
        read *, file
        call Save_grid(grid, trim(file))
        print *, "File saved."

      case (4)
        call print_validity(grid, "The grid to process is valid.", "The grid to process is invalid.")

      case (5)
        print *, "Below, the grid to process:"
        call Display_grid(grid)
        call print_validity(grid, "The grid is valid.", "The grid is invalid.")

      case (6)
        call cpu_time(Start)
        call CreateFilledGrid(grid)
        call Display_grid(grid)
        ! grid validation:
        call print_validity(grid, "The grid is valid.", "Computational error:  the grid is invalid!")
        call cpu_time(End)
        write (*, "(A, F12.3, A)") " Computing time: ", End - Start, " s."

      case (7)
        print *, "Below, the grid submitted:"
        call Display_grid(grid)
        call cpu_time(Start)
        call Solve_grid(grid)
        call print_validity(grid, "Below, the solved grid (validity was verified):", &
                                & "The initial grid was invalid, impossible to solve...")
        call Display_grid(grid)
        call cpu_time(End)
        write (*, "(A, F12.3, A)") " Computing time: ", End - Start, " s."

      case (8)
        print *, "How many digits in [17, 81] do you want in the puzzle grid?"
        print *, "Below 28, the computation can be longer!"
        read *, remainder
        call CreateFilledGrid(grid)
        print *, "Below, a filled grid:"
        call Display_grid(grid)
        ! grid validation:
        call print_validity(grid, "The grid is valid.", "The grid is invalid: problem to compute a solution!")

        call cpu_time(Start)
        call CreateSudokuGrid(grid, remainder)
        print *, "Below a Sudoku puzzle (with probably a unique solution):"
        call Display_grid(grid)
        call print_validity(grid, "valid grid", "Invalid grid: problem to compute a solution!")
        call cpu_time(End)
        write (*, "(A, F12.3, A)") " Computing time: ", End - Start, " s."

      case (9)
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

    if (ValidGrid(grid)) then
      print *, yes
    else
      print *, no
    end if
  end subroutine
end program main

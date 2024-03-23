!! This file is part of the ForSudoku Fortran project.
!! Copyright (C) 2006-2024 Vincent Magnin & Norwid Behrnd
!!
!! This is free software; you can redistribute it and/or modify
!! it under the terms of the GNU General Public License as published by
!! the Free Software Foundation; either version 3, or (at your option)
!! any later version.
!!
!! This software is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU General Public License for more details.
!!
!! You should have received a copy of the GNU General Public License along with
!! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
!! If not, see <http://www.gnu.org/licenses/>.
!!------------------------------------------------------------------------------
!! Contributed by Vincent Magnin, 2006-11-27; Norwid Behrnd, 2023
!! Last modifications: 2023-09-12, vmagnin 2024-03-23
!!------------------------------------------------------------------------------

module sudoku
  !! Sudoku module
  !!
  !! @author: Vincent Magnin and Norwid Behrnd
  !!
  !! This module contains a set of procedures to generate sudokus grids and
  !! solve them.
  implicit none

contains

  !*****************************************************************************
  ! Input/Output routines
  !*****************************************************************************
  subroutine save_grid(grid, filename)
    !! Input/Output routines
    integer, dimension(9, 9), intent(in) :: grid
    character(*), intent(in) :: filename

    integer :: row, col
    integer :: fileunit

    ! file creation:
    open (newunit=fileunit, file=filename, status="REPLACE")

    do row = 1, 9
      write (fileunit, '(3I2, " |", 3I2, " |", 3I2)') (grid(row, col), col=1, 9)
      if ((row == 3) .or. (row == 6)) then
        write (fileunit, *) "------+-------+------"
      end if
    end do

    close (fileunit)
  end subroutine save_grid

  subroutine read_grid(grid, filename)
    !! Read a grid from a provided the path to a file
    integer, dimension(9, 9), intent(out) :: grid !! Output grid
    character(*), intent(in) :: filename !! File path

    character(len=2) :: pipe1, pipe2   ! to read the pipe/the vertical bar
    integer :: row
    integer :: fileunit
    logical :: file_exists  ! check for the presence of the file requested

    inquire (file=filename, exist=file_exists)
    if (file_exists .eqv. .false.) stop "The requested file is absent."

    ! open and read the file, line by line
    open (newunit=fileunit, file=filename)

    do row = 1, 9
      read (fileunit, '(3I2, A2, 3I2, A2, 3I2)') &
        grid(row, 1:3), pipe1, grid(row, 4:6), pipe2, grid(row, 7:9)

      ! skip the lines of dashes
      if ((row == 3) .or. (row == 6)) then
        read (fileunit, *)
      end if
    end do

    close (fileunit)
  end subroutine read_grid

  subroutine display_grid(grid)
    !! Display a grid on the terminal.
    integer, dimension(9, 9), intent(in) :: grid

    integer :: row, col

    do row = 1, 9
      print '(3I2, " |", 3I2, " |", 3I2)', (grid(row, col), col=1, 9)
      if ((row == 3) .or. (row == 6)) then
        print *, "------+-------+------"
      end if
    end do
  end subroutine display_grid

  subroutine request_grid(grid)
    integer, dimension(9, 9), intent(inout) :: grid

    integer :: row, col

    do row = 1, 9
      write (*, "(A, I1, A)") "Enter line ", row, ":"
      read *, (grid(row, col), col=1, 9)
    end do
  end subroutine request_grid

  !*****************************************************************************
  ! Validation routines
  !*****************************************************************************
  pure logical function valid_colum_or_row(vector)
    !! Validation of either a row or a column.
    !!
    !! Returns true if each digit in the 1D array appears only once
    integer, dimension(1:9), intent(in) :: vector !! A row or a column

    ! The number of occurrences of each digit:
    integer, dimension(1:9) :: counters
    integer :: i

    counters = 0
    do i = 1, 9
      associate(d => vector(i))
      if (d /= 0) then
        counters(d) = counters(d) + 1
        if (counters(d) > 1) then
          valid_colum_or_row = .false.
          return        ! We leave immediately the function and return false
        end if
      end if
      end associate
    end do

    valid_colum_or_row = .true.
  end function valid_colum_or_row

  pure logical function valid_zone(region)
    !! Validation of a zone/region.
    !!
    !! Returns true if each digit in the 3x3 region appears only once.
    integer, dimension(1:3, 1:3), intent(in) :: region !! Sudoku's subregion

    ! The number of occurrences of each digit:
    integer, dimension(1:9) :: counters
    integer :: row, col

    counters = 0
    do row = 1, 3
      do col = 1, 3
        associate(d => region(row, col))
        if (d /= 0) then
          counters(d) = counters(d) + 1
          if (counters(d) > 1) then
            valid_zone = .false.
            return        ! We leave immediately the function and return false
          end if
        end if
        end associate
      end do
    end do

    valid_zone = .true.
  end function valid_zone

  pure logical function valid_grid(grid)
    !! Check if the whole grid is valid.
    !!
    !! Returns true if a full grid is valid.
    integer, dimension(9, 9), intent(in) :: grid !! Sudoku grid.

    integer :: row, col

    ! Verification of the 9 lines:
    do row = 1, 9
      if (.not. valid_colum_or_row(grid(row, 1:9))) then
        valid_grid = .false.
        return
      end if
    end do

    ! Verification of the 9 columns:
    do col = 1, 9
      if (.not. valid_colum_or_row(grid(1:9, col))) then
        valid_grid = .false.
        return
      end if
    end do

    ! Verification of the 9 regions:
    do row = 1, 7, +3
      do col = 1, 7, +3
        if (.not. valid_zone(grid(row:row+2, col:col+2))) then
          valid_grid = .false.
          return
        end if
      end do
    end do

    valid_grid = .true.
  end function valid_grid

  pure logical function valid_digit(grid, row, col)
    !! Returns true if the row, column and region of a digit are all valid:
    integer, dimension(9, 9), intent(in) :: grid !! Sudoky grid.
    integer, intent(in) :: row !! Row number of the region.
    integer, intent(in) :: col !! Column number of the region.

    integer :: i, j
    i = (row - 1) / 3
    j = (col - 1) / 3

    valid_digit = valid_colum_or_row(grid(row, 1:9)) .and. &
                  valid_colum_or_row(grid(1:9, col)) .and. &
                  valid_zone(grid(i*3+1:i*3+3, j*3+1:j*3+3))
  end function valid_digit

  pure logical function is_full(grid)
    !! Returns true if the grid is full.
    integer, dimension(9, 9), intent(in) :: grid !! Sudoky grid.

    if (any(grid(:,:) == 0)) then
      is_full = .false.
    else
      is_full = .true.
    end if
  end function

  pure subroutine list_possible_digits(grid, row, col, &
                                  nb_possible, possible_digit)
    !! Procedure to create a list of allowed digits in the present empty cell.
    integer, dimension(9, 9), intent(in) :: grid !! Sudoku grid
    integer, intent(in) :: row !! Row number
    integer, intent(in) :: col !! Column number
    ! These arguments are returned:
    integer, intent(out) :: nb_possible
    integer, dimension(1:9), optional, intent(out) :: possible_digit

    integer :: cr, lr, i, j
    logical, dimension(0:9) :: possible  ! Each digit is either possible or not

    possible = .true.

    ! Given digits in those row and column are excluded:
    do j = 1, 9
      possible(grid(j, col)) = .false.
      possible(grid(row, j)) = .false.
    end do

    ! Given digits in that region are excluded:
    lr = 1 + 3 * ((row - 1) / 3)
    cr = 1 + 3 * ((col - 1) / 3)
    do i = lr, lr + 2
      do j = cr, cr + 2
        possible(grid(i, j)) = .false.
      end do
    end do

    nb_possible = 0
    if (present(possible_digit)) possible_digit = 0
    ! Count and store the remaining possible digits:
    do j = 1, 9
      if (possible(j)) then
        nb_possible = nb_possible + 1
        if (present(possible_digit)) possible_digit(nb_possible) = j
      end if
    end do
  end subroutine list_possible_digits

  !*****************************************************************************
  ! Solver routines
  !*****************************************************************************

  pure subroutine sort(empty_cells, p, n)
    !! Starting from position p, sort the list of empty cells by
    !! ascending number of allowed digits. We use a bubble sort algorithm
    integer, dimension(1:81, 1:3), intent(inout) :: empty_cells !!
    integer, intent(in) :: p !! The sort starts at position p (included)
    integer, intent(in) :: n !! Number of empty cells in the list

    integer :: i
    integer, dimension(1:3) :: col
    logical :: none_swap

    do
      ! Compare each cell with the next one and swap them if necessary:
      none_swap = .true.
      do i = p, n - 1
        if (empty_cells(i, 3) > empty_cells(i+1, 3)) then
          ! Swap them:
          col = empty_cells(i, :)
          empty_cells(i  , :) = empty_cells(i+1, :)
          empty_cells(i+1, :) = col
          none_swap = .false.
        end if
      end do

      if (none_swap) exit     ! The bubble sort is finished
    end do
  end subroutine sort

  subroutine solve_puzzle(grid)
    !! Receives a puzzle grid and solves it.
    integer, dimension(9, 9), intent(inout) :: grid
      !! Input problem grid and returns solved grid.

    integer, dimension(9, 9) :: grid0
    real    :: r   ! Random number
    integer :: row, col, i, j
    ! Counter of empty cells:
    integer :: nb_empty
    ! List of empty cells:
    integer, dimension(1:81, 1:3) :: empty_cells
    ! List and number of possible digits:
    integer, dimension(1:9) :: possible_digit
    integer :: nb_possible

    ! Save the initial grid:
    grid0 = grid

    ! Identify and store the coordinates of empty cells in the grid
    ! in the table "empty_cells":
    empty_cells = 0
    nb_empty = 0
    do row = 1, 9
      do col = 1, 9
        if (grid(row, col) == 0) then
          nb_empty = nb_empty + 1
          empty_cells(nb_empty, 1) = row
          empty_cells(nb_empty, 2) = col
        end if
      end do
    end do

    ! Iterate over all empty cells:
    possible_digit = 0
    i = 1
    do while (i <= nb_empty)
      ! To accelerate the algorithm, count for each empty cell the digits
      ! which could be inserted in that cell:
      do j = i, nb_empty
        row = empty_cells(j, 1)
        col = empty_cells(j, 2)
        ! The last two arguments have intent(out):
        call list_possible_digits(grid, row, col, empty_cells(j, 3))
        ! empty_cells(j, 3) will contain the nb of possible digits for the
        ! empty cell number j.
      end do

      ! Sort the empty cells:
      call sort(empty_cells, i, nb_empty)

      ! For the empty cell i, regenerate a list of possible digits:
      row = empty_cells(i, 1)
      col = empty_cells(i, 2)
      call list_possible_digits(grid, row, col, &
                              & nb_possible, possible_digit)

      ! If there are possibilities, choose randomly one and
      ! continue with the next empty cell:
      if (nb_possible > 0) then
        call random_number(r)     ! 0 <= r < 1
        grid(row, col) = possible_digit(1 + int(r * nb_possible))
        i = i + 1
      else ! Start all over again
        i = 1
        grid = grid0
      end if
    end do
  end subroutine solve_puzzle

  subroutine cli_solver(grid, file)
    !! Provides a solution for a puzzle passed by CLI:
    !!
    !! ```shell
    !! $ ./executable test_in_02.txt
    !! ```
    integer, dimension(9, 9), intent(inout) :: grid !! Sudoku Grid
    character(*), intent(in) :: file !! Filepath

    logical :: presence
    presence = .false.
    inquire (file=file, exist=presence)

    if (presence .eqv. .false.) then
      print *, "ERROR: the requested file '", file, "' is inaccessible."
    else
      call read_grid(grid, file)

      if (valid_grid(grid) .eqv. .true.) then
        call solve_puzzle(grid)
        call display_grid(grid)
      else
        print *, "The input file'", file, "' is an invalid grid."
      end if
    end if
  end subroutine cli_solver

  !*****************************************************************************
  ! Puzzle generators
  !*****************************************************************************

  subroutine create_filled_grid(grid)
    !! Grid generation by brute force: in each cycle a digit is added and
    !! checked for validity.  If the grid becomes invalid, the grid generation
    !! is started all over again.
    integer, dimension(9, 9), intent(out) :: grid !! Sudoku grid

    real    :: r
    integer :: row, col
    integer :: tests

    restart:do
      ! We start with an empty grid:
      grid = 0

      try:do row = 1, 9
        do col = 1, 9
          tests = 0

          digit: do
            tests = tests + 1
            ! We add a random digit in the grid:
            call random_number(r)
            grid(row, col) = 1 + int(r * 9)
            ! and check if the Sudoku constraints are OK:
            if (valid_digit(grid, row, col)) then
              ! Let's continue with other cells:
              exit digit
            else
              if (tests > 30) then
                ! The probability of finding a valid digit is low,
                ! and we therefore restart a new grid:
                cycle restart
              end if
            end if
          end do digit
        end do
      end do try
      ! We have left naturally the "try" loop 
      ! and have therefore found a valid grid:
      exit
    end do restart
  end subroutine create_filled_grid

  subroutine create_puzzle_with_unique_solution(grid, nb_empty)
    !! Creates a minimal puzzle.
    !! Digits are randomly removed one by one. The process ends when it is not
    !! possible anymore to remove a digit while keeping a unique solution.
    !! The number of remaining digits is therefore a priori unknown.
    integer, dimension(9, 9), intent(inout) :: grid
    integer, intent(out) :: nb_empty

    ! List of the cells, numbered from 1 to 81, line by line:
    integer, dimension(81) :: list
    real    :: r(2)   ! To draw two random numbers
    integer :: row, col, n, n1, n2, i, temp, d
    integer :: nb_possible

    ! List of the cells, numbered from 1 to 81, line by line:
    list = [(i, i=1,81)]

    ! The list is randomly shuffled to avoid removing too many neighbours.
    ! The probability that a position is never drawn is (80/81)^81 ~ 0.107
    ! Increasing the upper limit would impede performance.
    do i = 1, 81
      ! We draw two positions:
      call random_number(r)     ! 0 <= r < 1
      n1 = 1 + int(r(1) * 81)
      n2 = 1 + int(r(2) * 81)
      ! and swap them in the list:
      temp     = list(n1)
      list(n1) = list(n2)
      list(n2) = temp
    end do

    nb_empty = 0
    ! Remove digits one by one:
    do i = 1, 81
      ! Number of the cell in the shuffled list:
      n = list(i)
      ! Coordinates of the cell in the grid:
      row = 1 + (n-1) / 9
      col = 1 + mod(n-1, 9)
      ! We save then delete the digit in that cell:
      d = grid(row, col)
      grid(row, col) = 0
      ! How many digits are possible at that position?
      ! Note: 79% of CPU time is spent in list_possible_digits()
      call list_possible_digits(grid, row, col, nb_possible)
      if (nb_possible > 1) then
        ! We put back the digit in the cell:
        grid(row, col) = d
        ! and we continue with the next cell in the list...
      else
        nb_empty = nb_empty + 1
      end if
    end do
  end subroutine create_puzzle_with_unique_solution

  subroutine create_puzzle(grid, givens)
    !! Creates a puzzle by brute force.
    !! But we are not 100% sure that the solution is unique
    !! (just a "high" probability).
    integer, dimension(9, 9), intent(inout) :: grid !! Sudoku grid.
    integer, intent(in) :: givens !! Number of given digits in the puzzle.

    integer, dimension(9, 9) :: grid0
    ! Maximum number of times we try to solve a grid:
    integer, parameter :: n = 1000
    ! To store and compare the n Sudoku solutions:
    integer, dimension(:, :, :), allocatable :: solutions
    real    :: r(2)
    integer :: row, col, i
    logical :: unique

    allocate(solutions(1:n, 1:9, 1:9))

    ! Save the initial grid:
    grid0 = grid

    print *, "Search of a puzzle (without guaranty for a unique solution)..."

    do
      grid = grid0
      ! Show the advancement of the algorithm:
      write(*, '(".")', advance='no')

      ! Remove digits:
      do i = 1, 81 - givens
        ! Choose randomly a cell with a digit:
        do
          call random_number(r)
          row = 1 + int(r(1) * 9)
          col = 1 + int(r(2) * 9)

          if (grid(row, col) /= 0) exit
        end do
        ! Erase the digit in this cell:
        grid(row, col) = 0
      end do

      ! The grid is solved up to n times to increase the probability that
      ! the solution is unique:
      unique = .true.
      solve: do i = 1, n
        solutions(i, :, :) = grid
        call solve_puzzle(solutions(i, :, :))

        ! Is that solution identical to all previous ones?
        if (i >= 2) then
          if (any(solutions(i, :, :) /= solutions(i-1, :, :))) then
            unique = .false.
            exit solve
          end if
        end if
      end do solve

      if (unique) exit
    end do
    write(*,*)
  end subroutine create_puzzle

  !**************************************************************
  ! System independent initialization of pseudo-random generator
  !**************************************************************
  subroutine initialize_random_number_generator(user_seed)
    !! Initialize random number generator with a seed.
    integer, optional, intent(in)      :: user_seed
    integer, allocatable, dimension(:) :: seed
    integer, dimension(1:8)            :: time_values
    integer :: i, n

    call random_seed(size=n)
    allocate (seed(1:n))

    if (present(user_seed)) then
      seed = user_seed
    else
      ! Real-time clock:
      call date_and_time(values=time_values)
      ! We use the milliseconds to compute the seeds:
      do i = 1, n
        seed(i) = (huge(seed(i)) / 1000) * time_values(8) - i
      end do
    end if

    call random_seed(put=seed(1:n))
  end subroutine initialize_random_number_generator

end module sudoku

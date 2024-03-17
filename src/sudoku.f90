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
! Last modifications: 2023-09-12, vmagnin 2024-03-17
!------------------------------------------------------------------------------

module sudoku
  implicit none

contains

  ! Receives a puzzle grid and solves it:
  subroutine solve_puzzle(grid)
    integer, dimension(9, 9), intent(inout) :: grid

    integer, dimension(9, 9) :: grid0
    real    :: r   ! Random number
    integer :: row, col, i, j
    ! Counter of empty/non allocated cells:
    integer :: nb_empty
    ! List of empty cells:
    integer, dimension(1:81, 1:3) :: empty_cells
    ! List and number of (still) possible digits:
    integer, dimension(1:9) :: possible_digit
    integer :: nb_possible

    ! save the initial grid:
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
        call list_possible_digits(grid, row, col, &
                                & empty_cells(j, 3), possible_digit)
        ! empty_cells(j, 3) will contain the nb of possible digits for the
        ! empty cell number j and the array possible_digit(1:9)
        ! the corresponding digits (but that array is not used for sorting).
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

  ! Procedure to create a list of allowed digits in the present empty cell:
  subroutine list_possible_digits(grid, row, col, &
                                  nb_possible, possible_digit)
    integer, dimension(9, 9), intent(in) :: grid
    integer, intent(in) :: row, col
    ! These arguments are returned:
    integer, intent(out) :: nb_possible
    integer, dimension(1:9), intent(out) :: possible_digit

    integer :: cr, lr, i, j
    logical, dimension(0:9) :: possible  ! Plausibility of each digit

    possible = .true.

    ! Digits already present in those row and column are excluded:
    do j = 1, 9
      possible(grid(j, col)) = .false.
      possible(grid(row, j)) = .false.
    end do

    ! Digits already present in that region are excluded:
    lr = 1 + 3 * ((row - 1) / 3)
    cr = 1 + 3 * ((col - 1) / 3)
    do i = lr, lr + 2
      do j = cr, cr + 2
        possible(grid(i, j)) = .false.
      end do
    end do

    nb_possible = 0
    possible_digit = 0
    ! Count and store the remaining possible digits:
    do j = 1, 9
      if (possible(j)) then
        nb_possible = nb_possible + 1
        possible_digit(nb_possible) = j
      end if
    end do
  end subroutine list_possible_digits

  ! Starting from position p, sort the (still) empty cells by
  ! ascending number of allowed digits. We use a bubble sort:
  subroutine sort(empty_cells, p, n)
    integer, dimension(1:81, 1:3), intent(inout) :: empty_cells
    integer, intent(in) :: p    ! The sort starts at position p (included)
    integer, intent(in) :: n    ! Number of empty lists

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

  ! Grid generation by brute force: in each cycle a digit is added and checked
  ! for validity.  If the grid became invalid, the grid generation
  ! is started all over again.
  subroutine create_filled_grid(grid)
    integer, dimension(9, 9), intent(out) :: grid

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

  ! Returns true if the row, column and region of a cell are all valid:
  pure logical function valid_digit(grid, row, col)
    integer, dimension(9, 9), intent(in) :: grid
    integer, intent(in) :: row, col

    integer :: i, j
    i = (row - 1) / 3
    j = (col - 1) / 3

    valid_digit = valid_colum_or_row(grid(row, 1:9)) .and. &
                  valid_colum_or_row(grid(1:9, col)) .and. &
                  valid_zone(grid(i*3+1:i*3+3, j*3+1:j*3+3))
  end function valid_digit

  ! Creates a Sudoku puzzle by brute force.
  ! But we are not 100% sure that the solution is unique
  ! (just a high probability).
  subroutine create_puzzle(grid, remainder)
    integer, dimension(9, 9), intent(inout) :: grid
    integer, intent(in) :: remainder

    integer, dimension(9, 9) :: grid0
    ! Maximum number of times we try to solve a grid:
    integer, parameter :: n = 1000
    ! To store and compare the n solutions:
    integer, dimension(:, :, :), allocatable :: solutions
    real    :: r(2)
    integer :: row, col, i
    logical :: unique

    allocate(solutions(1:n, 1:9, 1:9))

    ! Save the initial grid:
    grid0 = grid

    print *, "Search of a grid with a probably unique solution..."

    do
      grid = grid0
      ! Show the advancement of the algorithm:
      write(*, '(".")', advance='no')

      ! Remove digits:
      do i = 1, 81 - remainder
        ! Choose randomly a non-empty cell:
        do
          call random_number(r)
          row = 1 + int(r(1) * 9)
          col = 1 + int(r(2) * 9)

          if (grid(row, col) /= 0) exit
        end do
        ! Erase the digit in this cell:
        grid(row, col) = 0
      end do

      ! The grid is solved up to n times to evaluate the probability that
      ! the solution be unique:
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

  !*****************************************************************************
  ! Input/Output routines
  !*****************************************************************************
  subroutine save_grid(grid, filename)
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
    integer, dimension(9, 9), intent(out) :: grid
    character(*), intent(in) :: filename

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
  ! Returns true if each digit in the 1D array appears only once:
  pure logical function valid_colum_or_row(vector)
    integer, dimension(1:9), intent(in) :: vector   ! A row or a column

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

  ! Returns true if each digit in the 3x3 region appears only once.
  pure logical function valid_zone(region)
    integer, dimension(1:3, 1:3), intent(in) :: region

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

  ! Returns true if a full grid is valid:
  pure logical function valid_grid(grid)
    integer, dimension(9, 9), intent(in) :: grid

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

  ! Returns true if the grid is full:
  pure logical function is_full(grid)
    integer, dimension(9, 9), intent(in) :: grid

    if (any(grid(:,:) == 0)) then
      is_full = .false.
    else
      is_full = .true.
    end if
  end function

  !**************************************************************
  ! System independent initialization of pseudo-random generator
  !**************************************************************
  subroutine initialize_random_number_generator
    integer, dimension(1:8) :: time_values
    integer, allocatable, dimension(:) :: seed
    integer :: i, n

    call random_seed(size=n)
    allocate (seed(1:n))

    ! Real-time clock:
    call date_and_time(values=time_values)
    ! We use the milliseconds to compute the seeds:
    do i = 1, n
      seed(i) = (huge(seed(i)) / 1000) * time_values(8) - i
    end do

    ! Uncomment this line if you always need the same pseudo-random sequence
    ! for debugging or testing:
    !seed = 0

    call random_seed(put=seed(1:n))
  end subroutine initialize_random_number_generator


  subroutine solver(grid, file)
    ! ******************************************************************
    ! Provides a solution for a partially filled grid entered as a file
    !
    ! Concept study for a direct invocation of the executable by the CLI
    ! as, for example, by
    !
    ! ```shell
    ! $ ./executable test_in_02.txt
    ! ```
    !
    ! ******************************************************************
    integer, dimension(9, 9), intent(inout) :: grid
    character(len=50), intent(in) :: file

    logical :: presence
    presence = .false.

    inquire (file=file, exist=presence)
    if (presence .eqv. .false.) then
      print *, "The requested file '", trim(file), "' is inaccessible."
    end if

    call read_grid(grid, file)

    if (valid_grid(grid) .eqv. .true.) then
      call solve_puzzle(grid)
      call display_grid(grid)
    else
      print *, "The input by file'", trim(file), "' is an invalid grid."
    end if

  end subroutine solver
end module sudoku

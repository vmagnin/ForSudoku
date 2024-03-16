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
! Last modifications: 2023-09-12, vmagnin 2024-03-16
!------------------------------------------------------------------------------

module sudoku
  use iso_fortran_env, only: dp => real64
  implicit none

contains

  ! Receives a puzzle grid and solves it:
  subroutine Solve_grid(grid)
    integer, dimension(9, 9), intent(inout) :: grid

    integer, dimension(9, 9) :: grid_0
    real(dp) :: r   ! Random number
    integer :: row, col, i, j
    ! Counter of empty/non allocated cells:
    integer :: nb_empty
    ! List of empty cells:
    integer, dimension(1:81, 1:3) :: empty_cells
    ! List and number of (still) possible digits:
    integer, dimension(1:9) :: possible_digit
    integer :: nb_possible

    ! save the initial grid:
    grid_0 = grid

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
        grid = grid_0
      end if
    end do
  end subroutine Solve_grid

  ! Procedure to create a list of allowed digits in the present empty cell:
  subroutine list_possible_digits(grid, row0, col0, &
                                  nb_possible, possible_digit)
    integer, dimension(9, 9), intent(in) :: grid
    integer, intent(in) :: row0, col0
    ! These arguments are returned:
    integer, intent(out) :: nb_possible
    integer, dimension(1:9), intent(out) :: possible_digit

    integer :: row, col, cr, lr, j
    logical, dimension(0:9) :: possible  ! Plausibility of each digit

    possible = .true.

    ! Digits already present in those row and column are excluded:
    do j = 1, 9
      possible(grid(j, col0)) = .false.
      possible(grid(row0, j)) = .false.
    end do

    ! Digits already present in that region are excluded:
    lr = 1 + 3 * ((row0 - 1) / 3)
    cr = 1 + 3 * ((col0 - 1) / 3)
    do row = lr, lr + 2
      do col = cr, cr + 2
        possible(grid(row, col)) = .false.
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
  subroutine CreateFilledGrid(grid)
    integer, dimension(9, 9), intent(out) :: grid

    real(dp) :: r
    integer  :: row, col
    integer  :: tests

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
            grid(row, col) = 1 + int(r * 9_dp)
            ! and check if the Sudoku constraints are OK:
            if (ValidDigit(grid, row, col)) then
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
  end subroutine CreateFilledGrid

  ! Returns true if the row, column and region of a cell are all valid:
  pure logical function ValidDigit(grid, row, col)
    integer, dimension(9, 9), intent(in) :: grid
    integer, intent(in) :: row, col

    integer :: i, j
    i = (row - 1) / 3
    j = (col - 1) / 3

    ValidDigit = ValidColumOrRow(grid(row, 1:9)) .and. &
                 ValidColumOrRow(grid(1:9, col)) .and. &
                 ValidZone(grid(i*3+1:i*3+3, j*3+1:j*3+3))
  end function ValidDigit

  ! Note: at present it is unknown if there are Sudoku grids with less than
  ! 17 non-zero cells yield a unique solution.
  subroutine CreateSudokuGrid(grid, remainder)
    integer, dimension(9, 9), intent(inout) :: grid
    integer, intent(in) :: remainder

    integer, parameter :: n = 1000    ! Nb of times we try to solve a grid
    integer, dimension(9, 9) :: grid_0
    integer, dimension(1:n, 1:9, 1:9) :: solutions
    real(kind=dp) :: random
    integer :: row, col, i
    logical :: empty, unique

    ! save the initial grid:
    grid_0 = grid

    print *, "Search of a grid with a probably unique solution ..."
    unique = .false.
    do while (.not. unique)
      grid = grid_0

      ! remove randomly empty cells
      do i = 1, 81 - remainder
        ! by chance, one picks a of the cells to be removed:
        empty = .false.
        do while (.not. empty)
          call random_number(random)
          row = 1 + int(random * 9_dp)
          call random_number(random)
          col = 1 + int(random * 9_dp)
          if (grid(row, col) /= 0) then
            empty = .true.
          end if
        end do
        ! erase the previously assigned digit in this cell:
        grid(row, col) = 0
      end do

      ! The grid is solved up to n times to try to see if the solution is unique
      unique = .true.
      i = 1
      sol: do while ((i <= n) .and. unique)
        solutions(i, :, :) = grid
        call Solve_grid(solutions(i, :, :))

        if (i >= 2) then
          if (any(solutions(i, :, :) /= solutions(i-1, :, :))) then
            unique = .false.
            exit sol
          end if
        end if

        i = i + 1
      end do sol

      ! Show the advancement of the algorithm:
      write(*, '(".")', advance='no')
    end do
    write(*,*)
  end subroutine CreateSudokuGrid

  !*****************************************************************************
  ! Input/Output routines
  !*****************************************************************************
  subroutine Save_grid(grid, filename)
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
  end subroutine Save_grid

  subroutine Read_grid(grid, filename)
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
  end subroutine Read_grid

  subroutine Display_grid(grid)
    integer, dimension(9, 9), intent(in) :: grid

    integer :: row, col

    do row = 1, 9
      print '(3I2, " |", 3I2, " |", 3I2)', (grid(row, col), col=1, 9)
      if ((row == 3) .or. (row == 6)) then
        print *, "------+-------+------"
      end if
    end do
  end subroutine Display_grid

  subroutine Request_grid(grid)
    integer, dimension(9, 9), intent(inout) :: grid

    integer :: row, col

    do row = 1, 9
      write (*, "(A, I1, A)") "Enter line ", row, ":"
      read *, (grid(row, col), col=1, 9)
    end do
  end subroutine Request_grid

  !*****************************************************************************
  ! Validation routines
  !*****************************************************************************
  ! Returns true if each digit in the 1D array appears only once:
  pure logical function ValidColumOrRow(vector)
    integer, dimension(1:9), intent(in) :: vector   ! A row or a column

    ! The number of occurrences of each digit:
    integer, dimension(1:9) :: counters
    integer :: i, d

    counters = 0
    do i = 1, 9
      associate(d => vector(i))
      if (d /= 0) then
        counters(d) = counters(d) + 1
        if (counters(d) > 1) then
          ValidColumOrRow = .false.
          return        ! We leave immediately the function and return false
        end if
      end if
      end associate
    end do

    ValidColumOrRow = .true.
  end function ValidColumOrRow

  ! Returns true if each digit in the 3x3 region appears only once.
  pure logical function ValidZone(region)
    integer, dimension(1:3, 1:3), intent(in) :: region

    ! The number of occurrences of each digit:
    integer, dimension(1:9) :: counters
    integer :: row, col, d

    counters = 0
    do row = 1, 3
      do col = 1, 3
        associate(d => region(row, col))
        if (d /= 0) then
          counters(d) = counters(d) + 1
          if (counters(d) > 1) then
            ValidZone = .false.
            return        ! We leave immediately the function and return false
          end if
        end if
        end associate
      end do
    end do

    ValidZone = .true.
  end function ValidZone

  ! Returns true if a full grid is valid:
  pure logical function ValidGrid(grid)
    integer, dimension(9, 9), intent(in) :: grid

    integer :: row, col

    ! Verification of the 9 lines:
    do row = 1, 9
      if (.not. ValidColumOrRow(grid(row, 1:9))) then
        ValidGrid = .false.
        return
      end if
    end do

    ! Verification of the 9 columns:
    do col = 1, 9
      if (.not. ValidColumOrRow(grid(1:9, col))) then
        ValidGrid = .false.
        return
      end if
    end do

    ! Verification of the 9 regions:
    do row = 1, 7, +3
      do col = 1, 7, +3
        if (.not. ValidZone(grid(row:row+2, col:col+2))) then
          ValidGrid = .false.
          return
        end if
      end do
    end do

    ValidGrid = .true.
  end function ValidGrid

  !************************************************************
  ! initialization of a system independent pseudorandom generator
  !************************************************************
  subroutine Initialize_Random
    integer(4), dimension(1:8) :: timeValues
    integer(4), allocatable, dimension(:) :: random_seede

    integer(4) :: loop, n

    call date_and_time(values=timeValues)

    ! retrieve the integers to store a seed: !? On récupère le nombre d'entiers servant à stocker la random_seede :
    call random_seed(size=n)
    allocate (random_seede(1:n))

    ! use thousandths of a second by the clock:
    do loop = 1, n
      random_seede(loop) = huge(random_seede(loop)) / 1000 * timeValues(8)
    end do

    ! hand over the seed:
    call random_seed(put=random_seede(1:n))
  end subroutine Initialize_Random


  subroutine solver(grid, file)
    ! ******************************************************************
    ! provide a solution for a partially filled grid entered as a file
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

    call Read_grid(grid, file)

    if (ValidGrid(grid) .eqv. .true.) then
      call Solve_grid(grid)
      call Display_grid(grid)
    else
      print *, "The input by file'", trim(file), "' is an invalid grid."
    end if

  end subroutine solver
end module sudoku

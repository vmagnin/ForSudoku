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
! Last modifications: 2023-09-12, vmagnin 2024-03-15
!------------------------------------------------------------------------------

module sudoku
  use iso_fortran_env, only: dp => real64
  implicit none

contains

  subroutine Solve_grid(grid)
    integer, dimension(9, 9), intent(inout) :: grid
    integer, dimension(9, 9) :: grid_0  ! empty grid
    real(kind=dp) :: random  ! random number
    integer :: row, column, line_0, row_0, i, j
    integer :: counter_empty_cells  ! counter of empty/non allocated cells
    integer, dimension(1:81, 1:3) :: empty_cells  ! list of empty cells
    ! logical, dimension(0:9)     :: possible
    integer, dimension(1:9) :: possible_digit  ! list of (still) possible digits
    integer :: counter_possible_digits

    possible_digit = 0

    ! save the initial grid:
    grid_0 = grid

    ! identify the grid coordinates of empty cells in the grid
    ! in a table of 81 entries
    empty_cells = 0
    counter_empty_cells = 0
    do row = 1, 9
      do column = 1, 9
        if (grid(row, column) == 0) then
          counter_empty_cells = counter_empty_cells + 1
          empty_cells(counter_empty_cells, 1) = row
          empty_cells(counter_empty_cells, 2) = column
        end if
      end do
    end do

    ! iterate over all empty cells:
    i = 1
    do while (i <= counter_empty_cells)
      ! To accelerate the algorithm, count for each empty cell the digits
      ! which yet possibly could be inserted in this very cell
      do j = i, counter_empty_cells
        line_0 = empty_cells(j, 1)
        row_0 = empty_cells(j, 2)
        call list_possible_digits(grid, line_0, row_0, &
                                  empty_cells(j, 3), possible_digit)
      end do
      ! retrieve the empty cells (which depends on the number of still
      ! possible digits)
      call Draw(empty_cells, i, counter_empty_cells)

      ! for cell (line_0,row_0), generate a list of possible digits:
      line_0 = empty_cells(i, 1)
      row_0 = empty_cells(i, 2)

      call list_possible_digits(grid, line_0, row_0, &
                                counter_possible_digits, possible_digit)

      ! if there are multiple possibilities, choose one (by chance) and
      ! continue with the next empty cell:
      if (counter_possible_digits > 1) then
        call random_number(random)
        j = 1 + int(random * counter_possible_digits)
        grid(line_0, row_0) = possible_digit(j)
        i = i + 1
        ! if there is only one possibility, use this digit now, and then
        ! continue with the next empty cell
      else if (counter_possible_digits == 1) then
        grid(line_0, row_0) = possible_digit(1)
        i = i + 1
        ! start all over again if there is none:
      else
        i = 1
        grid = grid_0
      end if
    end do
  end subroutine Solve_grid

  ! procedure to create a list of allowed digits in the present empty cell:
  subroutine list_possible_digits(grid, line_0, row_0, &
                                  counter_possible_digits, possible_digit)
    integer, dimension(9, 9), intent(in) :: grid
    integer, intent(in) :: line_0, row_0
    integer, intent(out) :: counter_possible_digits
    integer, dimension(1:9), intent(out) :: possible_digit

    integer :: row, column, cr, lr, j
    logical, dimension(0:9) :: possible  ! Plausibility of each digit

    possible = .true.
    do j = 1, 9
      possible(grid(j, row_0)) = .false.
      possible(grid(line_0, j)) = .false.
    end do

    lr = 1 + 3 * ((line_0 - 1) / 3)
    cr = 1 + 3 * ((row_0 - 1) / 3)
    do row = lr, lr + 2
      do column = cr, cr + 2
        possible(grid(row, column)) = .false.
      end do
    end do

    counter_possible_digits = 0
    possible_digit = 0
    do j = 1, 9
      if (possible(j)) then
        counter_possible_digits = counter_possible_digits + 1
        possible_digit(counter_possible_digits) = j
      end if
    end do
  end subroutine list_possible_digits

  !****************************************************************
  ! Starting from position p, sort the (still) empty cells by
  ! increasing number of allowed digits to them.  This is organized
  ! as a bubble sort.
  !****************************************************************
  subroutine Draw(empty_cells, p, n)
    integer, dimension(1:81, 1:3), intent(inout) :: empty_cells
    integer, intent(in) :: n    ! number of empty lists
    integer, intent(in) :: p    ! sort, start by position p (p inclusive)

    integer :: i, j ! loop counters
    integer, dimension(1:3) :: column
    logical :: completely_solved

    completely_solved = .false.
    do while (.not. completely_solved)
      completely_solved = .true.
      ! let's compare each cell with its succeeding cell
      do i = p, n - 1
        j = i + 1
        if (empty_cells(i, 3) > empty_cells(j, 3)) then
          ! exchange the two cases of this list:
          column = empty_cells(i, :)
          empty_cells(i, :) = empty_cells(j, :)
          empty_cells(j, :) = column
          completely_solved = .false.
        end if
      end do
    end do
  end subroutine

  ! Grid generation: in each cycle a digit is added and the grid is checked
  ! for validity.  If the grid became invalid, the grid generation is started
  ! all over again.
  ! With a  PIII 866 MHz: about 0.5 s.
  subroutine CreateFilledGrid(grid)
    integer, dimension(9, 9), intent(out) :: grid

    real(kind=dp) :: random
    integer :: row, column
    integer(4) :: tests
    logical :: completely_solved

    grid = 0

    row = 1
    do while (row <= 9)
      column = 1
      do while (column <= 9)
        tests = 0
        completely_solved = .false.
        do while (.not. completely_solved)
          if (tests > 30) then
            ! start from the very beginning
            ! (it were impossible to determine how many cycles one
            ! has to rewind to identify the erroneous one)
            grid = 0
            tests = 0
            column = 1
            row = 1
          end if
          tests = tests + 1
          call random_number(random)
          grid(row, column) = 1 + int(random * 9_dp)
          completely_solved = ValidDigit(grid, row, column)
        end do
        column = column + 1
      end do
      row = row + 1
    end do
  end subroutine CreateFilledGrid

  logical function ValidDigit(grid, row, column)
    integer, dimension(9, 9), intent(in) :: grid
    integer, intent(in) :: row, column

    integer :: i, j

    i = (row - 1) / 3
    j = (column - 1) / 3

    ValidDigit = ValidColumOrRow(grid(row, 1:9)) .and. &
                 ValidColumOrRow(grid(1:9, column)) .and. &
                 ValidZone(grid(i * 3 + 1:i * 3 + 3, j * 3 + 1:j * 3 + 3))
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
    integer :: row, column, i
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
          column = 1 + int(random * 9_dp)
          if (grid(row, column) /= 0) then
            empty = .true.
          end if
        end do
        ! erase the previously assigned digit in this cell:
        grid(row, column) = 0
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

    integer :: row, column
    integer :: fileunit, error

    ! file creation:
    open (newunit=fileunit, file=filename, status="REPLACE")

    do row = 1, 9
      write (fileunit, '(3I2, " |", 3I2, " |", 3I2)') (grid(row, column), column=1, 9)
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
    integer :: fileunit, error
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

    integer :: row, column

    do row = 1, 9
      print '(3I2, " |", 3I2, " |", 3I2)', (grid(row, column), column=1, 9)
      if ((row == 3) .or. (row == 6)) then
        print *, "------+-------+------"
      end if
    end do
  end subroutine Display_grid

  subroutine Request_grid(grid)
    integer, dimension(9, 9), intent(inout) :: grid

    integer :: row, column

    do row = 1, 9
      write (*, "(A, I1, A)") "Enter line ", row, ":"
      read *, (grid(row, column), column=1, 9)
    end do
  end subroutine Request_grid

  !*****************************************************************************
  ! Validation routines
  !*****************************************************************************
  ! Returns true if each digit in the 1D array appears only once:
  pure logical function ValidColumOrRow(vector)
    integer, dimension(1:9), intent(in) :: vector   ! A row or a column

    ! The number of occurrences of each digit:
    integer, dimension(1:9) :: counter
    integer :: i

    counter = 0
    do i = 1, 9
      if (vector(i) /= 0) then
        counter(vector(i)) = counter(vector(i)) + 1
        if (counter(vector(i)) > 1) then
          ValidColumOrRow = .false.
          return        ! We leave immediately the function and return false
        end if
      end if
    end do

    ValidColumOrRow = .true.
  end function ValidColumOrRow

  logical function ValidZone(region)
    integer, dimension(1:3, 1:3), intent(in) :: region

    integer, dimension(1:9) :: col

    col(1) = region(1, 1)
    col(2) = region(1, 2)
    col(3) = region(1, 3)
    col(4) = region(2, 1)
    col(5) = region(2, 2)
    col(6) = region(2, 3)
    col(7) = region(3, 1)
    col(8) = region(3, 2)
    col(9) = region(3, 3)
    if (ValidColumOrRow(col)) then
      ValidZone = .true.
    else
      ValidZone = .false.
    end if
  end function ValidZone

  logical function ValidGrid(grid)
    integer, dimension(9, 9), intent(in) :: grid

    integer :: row, column

    ValidGrid = .true.

    ! verification of lines:
    do row = 1, 9
      if (.not. ValidColumOrRow(grid(row, 1:9))) then
        ValidGrid = .false.
        return
      end if
    end do

    ! verification of columns:
    do column = 1, 9
      if (.not. ValidColumOrRow(grid(1:9, column))) then
        ValidGrid = .false.
        return
      end if
    end do

    ! verification of regions:
    do row = 1, 7, +3
      do column = 1, 7, +3
        if (.not. ValidZone(grid(row:row + 2, column:column + 2))) then
          ValidGrid = .false.
          return
        end if
      end do
    end do
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

# Changelog

All notable changes to the gtk-fortran project are documented in this file. The
format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [sudoku dev] 2024-03

### Added
- A logo in the `logo/` directory.
- Many tests.
- The function `is_full(grid)` returns true if the grid is full.
- Subroutine `create_puzzle_with_unique_solution(grid, nb_empty)`

### Changed
- The repository was renamed `https://github.com/vmagnin/ForSudoku` (instead of `sudoko`).
- Code improved, modernized and refactored.
- If a full valid grid is not in memory, a new one will be automatically
generated if you want to create a puzzle grid.
- `initialize_random_number_generator()` can now receive a seed from the main
program (useful for testing and debugging).

### Removed
- `Time()` function.


## [sudoku 0.8.1] 2023-07-22

- To accommodate users not familiar with the fpm based setup of the program,
  and more interested in the use of the program rather than a contribution to
  its development, the executable now equally can be obtained by a Makefile
  moderated instance of gfortran.
- To widen the potential audience of the users, the interface of the program
  was translated into English, and the documentation extended.  (While linting
  the source code, comments equally were translated, too.)
- The format to enter a partially filled grid manually, or from a text file,
  currently differ of each other.  A future version of the program possibly
  addresses this issue.
- The fpm environment allows both compilation and subsequent execution of
  Fortran source code, as well as testing of the procedures implemented.  The
  current version of the program does not yet use this quality control.


## [sudoku 0.8] 2006-11-27

- The original version.

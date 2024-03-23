# Changelog

All notable changes to the gtk-fortran project are documented in this file. The
format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [ForSudoku dev]

### Added
- A `Development` section in the `README.md`.
- `pure` attribute for `list_possible_digits()` and `sort()`.


## [ForSudoku 1.0.0] 2024-03-22

### Added
- A logo in the `logo/` directory.
- Many tests.
- The function `is_full(grid)` returns true if the grid is full.
- Subroutine `create_puzzle_with_unique_solution(grid, nb_empty)`
- More files containing grids in the `test` directory:
    - `16_givens_but_2_solutions.txt`
    - `77_givens_but_2_solutions.txt`
    - `a_puzzle_with_17_givens.txt`
    - `strangely_familiar.txt`
- More references in the `README.md`.

### Changed
- The repository was renamed `https://github.com/vmagnin/ForSudoku` (instead of `sudoko`). 
You can update your project with `git remote set-url origin git@github.com:vmagnin/ForSudoku.git`
- Code improved, modernized and refactored.
- The messages, comments and names are now following the Sudoku glossary.
- If a full valid grid is not in memory, a new one will be automatically
generated if you want to create a puzzle grid.
- The CLI solver `solver(grid, file)` was renamed `cli_solver(grid, file)`.
- `initialize_random_number_generator()` can now receive a seed from the main
program (useful for testing and debugging).
- `test/check.f90`: `assert_readtest01()` and `assert_readtest02` factorized in one `assert_readtest()` subroutine.

### Fixed
- A problem when reading a file path with option 2) of the menu. You can now type a relative path.

### Removed
- `Time()` function.


## [ForSudoku 0.8.1] 2023-07-22

- To accommodate users not familiar with the fpm based setup of the program,
  and more interested in the use of the program rather than a contribution to
  its development, the executable now equally can be obtained by a Makefile
  moderated instance of gfortran.
- To widen the potential audience of the users, the interface of the program
  was translated into English, and the documentation extended.  (While linting
  the source code, comments equally were translated, too.)
- The fpm environment allows both compilation and subsequent execution of
  Fortran source code, as well as testing of the procedures implemented.


## [ForSudoku 0.8] 2006-11-27

- The original version.

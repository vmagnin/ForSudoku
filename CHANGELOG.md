# Changelog

All notable changes to the gtk-fortran project are documented in this file. The
format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [sudoku dev] 2024-03

### Added
- Many tests.

### Changed
- Code improved, modernized and refactored.

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

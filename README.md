# A Fortran sudoku generator and solver

## Requirements and dependencies

You need:

* a modern Fortran compiler, for example GFortran or the Intel ifort/ifx
  compilers. See the [Fortran-lang.org compilers
  page](https://fortran-lang.org/compilers/) for other compilers.
* The Fortran Package Manager [fpm](https://fpm.fortran-lang.org/).

## Running the program

The recommended approach to yield and launch an executable of the program is to
use the interface `fpm` provides:

```shell
$ fpm run
sudoku.f90                             done.
libsudoku.a                            done.
main.f90                               done.
sudoku                                 done.
[100%] Project compiled successfully.
```

Especially if your current interest only aims to use the program, the included
`Makefile` equally provides you with the executable with no difference down the
road.

The program's main interface provides access to all principal functions:

```shell
 *************************** MENU *****************************************
 1) Manual input (lines of comma separated 1 - 9, or 0 (unallocated cell).)
 2) Input from a text file.  For permitted patterns, see the documentation.
 3) Save the currently processed grid as a text file.
 4) Check the validity of the grid currently stored in memory.
 5) Display the grid currently stored in memory.
 6) Create a random, already filled Sudoku grid.
 7) Solve the Sudoku grid currently stored in memory.
 8) Create a partially allocated grid (conjecture of a likely unique solution).
 9) Quit.
 Select one of them and click `Enter`:
```

### option 1, manual entry of a partially filled grid

The program can provide a solution for partially filled Sudoku grids.  In this
mode, sequentially enter cells one by one, starting from the left, as a comma
separated line.  Use any number `1` to `9` for known cells.  If the cell in
your reference pattern is still empty or unknown, enter either `0` (zero,
recommended), or a blank space instead.  After typing the last cell in the
current line (which is not followed by a comma), submit the line by `Enter`.

Upon completion of the input, the program quickly checks if the grid is valid
and displays it in a form similar to the one below:

```shell
 5 3 0 | 0 7 0 | 0 0 0
 6 0 0 | 1 9 5 | 0 0 0
 0 9 8 | 0 0 0 | 0 6 0
 ------+-------+------
 8 0 0 | 0 6 0 | 0 0 3
 4 0 0 | 8 0 3 | 0 0 1
 7 0 0 | 0 2 0 | 0 0 6
 ------+-------+------
 0 6 0 | 0 0 0 | 2 8 0
 0 0 0 | 4 1 9 | 0 0 5
 0 0 0 | 0 8 0 | 0 7 9
```

If the input is considered a valid one, use then option `7` to query for a
solution, i.e. a completely filled grid.

### option 2, file based entry of a partially filled grid

The present form of the program only reliably supports the input of a partially
filled grid if its own format depicted above.  Folder `check` contains example
files of a partially filled grid (`test_in_01.txt` and `test_in_02.txt`) as a
reference, and a grid filled with the explicit place holder zeroes to start
with (`template.txt`) for you.

If the input is considered a valid one, use then option `7` to query for a
solution, i.e. a completely filled grid.  For comparison, folder `check`
equally contains the corresponding solution of the test input (files
`test_out_01.txt` and `test_out_02.txt`) in the program's format which match
the topmost example in the English edition of Wikipedia.

## License

This project is licensed under the [GNU General Public License version 3 or
later](http://www.gnu.org/licenses/gpl.html).

## Bibliography

* <https://en.wikipedia.org/wiki/Sudoku>

![](logo/ForSudoku_logo.svg)

# A Fortran sudoku generator and solver

## Requirements and dependencies

You need:

* a modern Fortran compiler, for example GFortran or the Intel ifort/ifx
  compilers. See the [Fortran-lang.org compilers
  page](https://fortran-lang.org/compilers/) for other compilers.
* The Fortran Package Manager [fpm](https://fpm.fortran-lang.org/).

## Running the program

The recommended approach to yield and launch an executable of the program is to
use `fpm`:

```shell
$ fpm run
sudoku.f90                             done.
libForSudoku.a                         done.
main.f90                               done.
ForSudoku                              done.
[100%] Project compiled successfully.
```

If you want to generate a sudoku with many void cells, it is better to use the
optimization flags of your compiler, for example with GFortran:
```shell
$ fpm run --flag "-Ofast -static-libgfortran"
```

When using GFortran, you can also add the compiler flag `-Wno-array-temporaries`
to avoid the corresponding warnings.

With Intel compilers:
```shell
$ fpm run --compiler ifx --flag "-check:noarg_temp_created"
```

Especially if your current interest only aims to use the program, the included
`Makefile` equally provides you with the executable with no difference down the
road.

The program's main interface provides access to all principal functions:

```shell
 ******************************** MENU **************************************
 1) Manual input (lines of comma separated 1 - 9, or 0 (empty cell)).
 2) Read a grid from a text file (for permitted patterns, see the doc).
 3) Save the current grid in a text file.
 4) Check the validity of the current grid.
 5) Display the current grid.
 6) Create a random completed Sudoku grid.
 7) Solve the puzzle grid currently stored in memory.
 8) Create a minimal puzzle, starting from the grid in memory.
 9) Create a minimal puzzle with exactly n given digits.
 10) Create a puzzle grid (without guaranty for a unique solution).
 0) Quit.
```

### 1) Manual entry of a grid

In this mode, sequentially enter cells one by one, starting from the left, as a comma separated line.  Use any number `1` to `9` for known cells.  If the cell in your reference pattern is still empty or unknown, enter either `0` (zero,
recommended), or a blank space instead.  After typing the last cell in the
current line (which is not followed by a comma), submit the line by `Enter`:

```shell
Enter line 1:
1,2,3,4,5,6,7,8,9
```

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

If the input is considered a valid one, you can then use that grid with the other options.

### 2) Read a grid from a text file

If the file to read is not in the root directory of the project, type the relative path, for example `test/a_puzzle_with_17_givens.txt`. The folder `test/` also contains a few grids for testing. And you can create your own grid starting from `test/template.txt` file (a grid filled with zeroes).

The present form of the program only reliably supports the input of a grid in the format depicted above.

### 6) Generation of a completed Sudoku grid

The program generates a random completed Sudoku by brute force: in each cycle a digit is added and checked for validity. If the grid became invalid, the grid generation is started all over again, until a valid grid is obtained. But the process is very quick.

The new grid replaces the previous grid stored in memory.

### 7) Solve a puzzle grid.

The puzzle in memory is solved by brute force. But the process is very quick.

It is also possible to pass the filename of the puzzle to solve on the command line. With fpm you can type for example from the root directory of the project:

```shell
$ fpm run -- test/a_puzzle_with_17_givens.txt
```

If you have used the makefile, you can type:

```shell
$ ./executable test/a_puzzle_with_17_givens.txt
```

### 8) Create a minimal puzzle

If a completed valid grid is not in memory, we will start from a new one. As the algorithm uses random numbers to remove digits in the grid, following validity rules, the final number of given digits is a priori unknown. The process will be generally quick and the obtained puzzle will be most of the time half-empty.

### 9) Create a minimal puzzle with n given digits

You can ask the program any number of given digits in the [17, 81] range, but the computation will become longer below 33 as the algorithm is based on brute force: it launches the previous option until we obtain a puzzle with n given digits.

### 10) Create a puzzle grid (not guaranteed to have a unique solution)

You can obtain puzzles with still less given digits with this method, but without guaranty that the solution is unique.


## License

This project is licensed under the [GNU General Public License version 3 or
later](http://www.gnu.org/licenses/gpl.html). The logo files are under CC-BY-SA 4.0.

## Bibliography

* <https://en.wikipedia.org/wiki/Sudoku>
* <https://en.wikipedia.org/wiki/Glossary_of_Sudoku>
* <https://en.wikipedia.org/wiki/Mathematics_of_Sudoku>
* Jean-Paul Delahaye, ["The Science behind Sudoku"](https://www.cs.virginia.edu/~robins/The_Science_Behind_SudoKu.pdf), *Scientific American,*  Vol. 294, No. 6, pp. 80-87, June 2006, DOI:10.1038/scientificamerican0606-80. This is the English translation of:
    * Jean-Paul Delahaye, ["Le tsunami du Sudoku"](https://cristal.univ-lille.fr/profil/jdelahay/pls:2005:136.pdf), *Pour la Science,* n°338, p. 144-149, décembre 2005.
* Michael Metcalf. A Sudoku program in Fortran 95. *SIGPLAN Fortran Forum* 25,
1 (April 2006), 4–7. https://doi.org/10.1145/1124708.1124709
* Other [Fortran sudoku projects](https://github.com/search?q=sudoku%20fortran&type=repositories)
on GitHub.

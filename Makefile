#!/usr/bin/env make

# If fpm is unchartered territory for you, this GNU Makefile equally provides
# you the executable of the sudoku program.  Its execution depends on a recent
# version of gfortran (e.g., release 12.3.0).

default:
	gfortran -O3 -Wno-array-temporaries -c ./src/sudoku.f90
	gfortran -O3 -Wno-array-temporaries -c ./app/main.f90

	gfortran sudoku.o main.o -o executable

	rm sudoku.o main.o sudoku.mod

	./executable


# Provide an optional check: if `diff` reports the computed solution and the
# reference solution file as identical, the program probably is in good shape.
check:
	gfortran -O3 -c ./src/sudoku.f90
	gfortran -O3 -c ./app/main.f90

	gfortran *.o -o executable
	rm sudoku.o main.o sudoku.mod

	./executable ./test/test_in_01.txt > probe
	diff -s ./probe ./test/test_out_01.txt
	rm ./probe


# Assuming fprettify is available, apply the reformatter.
fprettify:
	fprettify -c ./fprettify.rc --recursive ./*
# END


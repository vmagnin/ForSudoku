! This file is part of the sudoku Fortran project.
! Copyright (C) 2006 Vincent MAGNIN
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
! Last modifications: 2023-07-25
!------------------------------------------------------------------------------

program main
    use iso_fortran_env, only: dp => real64
    use sudoku

    implicit none
    ! Variables locales :
    integer, dimension(1:9, 1:9) :: grille    ! (line, column)
    real(kind=dp)       :: Debut,Fin    ! monitor the duration of computation
    integer    :: choix
    integer    :: nvides       ! number of cells to clear
    character(50) :: fichier      ! name of file (including extension .txt)

    select case (command_argument_count())
    case (0) ! the typical invocation with `fpm run`

    ! initialize the pseudorandom number generator
    call Initialiser_Random
    ! initialize the grid (non allocated cells will be encoded by 0)
    grille = 0

    print *,"sudoku.f90, version 0.8.1, copyright (C) 2006 Vincent MAGNIN"
    ! infinite loop to provide the user a menu:
    do
        print *
        print *,"*************************** MENU *****************************************"
        print *,"1) Manual input (lines of comma separated 1 - 9, or 0 (unallocated cell).)"
        print *,"2) Input from a text file.  For permitted patterns, see the documentation."
        print *,"3) Save the currently processed grid as a text file."
        print *,"4) Check the validity of the grid currently stored in memory."
        print *,"5) Display the grid currently stored in memory."
        print *,"6) Create a random, already filled Sudoku grid."
        print *,"7) Solve the Sudoku grid currently stored in memory."
        print *,"8) Create a partially allocated grid (conjecture of a likely unique solution)."
        print *,"9) Quit."
        print *,"Select one of them and click `Enter`:"
        READ *,choix

        select case (choix)
        case (1)
            call Demander_grille(grille)
            print *,"You entered the following Sudoku:"
            call Afficher_grille(grille)
            if (GrilleValide(grille)) then
                print *, "The Sudoku is valid."
            else
                print *, "The Sudoku is invalid."
            end if
        case(2)
            print *,"Enter complete file name of the file to read (including .txt extension):"
            call SYSTEM("dir *.txt")
            READ *,fichier
            call Lire_grille(grille,trim(fichier))
            call Afficher_grille(grille)
            if (GrilleValide(grille)) then
                print *, "The Sudoku is valid."
            else
                print *, "The Sudoku is invalid."
            end if
        case(3)
            print *,"Enter complete file name of the file to save (incl. .txt):"
            READ *,fichier
            call Enregistrer_grille(grille,trim(fichier))
            print *,"File saved."
        case(4)
            if (GrilleValide(grille)) then
                print *, "The grid to process is valid."
            else
                print *, "The grid to process is invalid."
            end if
        case(5)
            print *,"Below, the grid to process:"
            call Afficher_grille(grille)
            if (GrilleValide(grille)) then
                print *, "The grid is valid."
            else
                print *, "The grid is invalid."
            end if
        case(6)
            Debut = Temps()
            call GenererGrillePleine(grille)
            call Afficher_grille(grille)
            ! grid validation:
            if (GrilleValide(grille)) then
                print *, "The grid is valid."
            else
                print *, "Computational error:  the grid is invalid!"
            end if
            Fin = Temps()
            print *,"computing time :", Fin-Debut, "s"
        case(7)
            print *,"Below, the grid submitted:"
            call Afficher_grille(grille)
            Debut = Temps()
            call ResoudreGrille(grille)
            if (GrilleValide(grille)) then
                print *, "Below, the solved grid (validity was verified):"
            else
                print *, "The initial grid was invalid, impossible to solve …"
            end if
            call Afficher_grille(grille)
            Fin = Temps()
            print *,"computing time", Fin-Debut, "s"
        case(8)
            print *,"How many numbers should be assigned in advance [17,81]?"
            print *,"Note: with less than 35 preallocated fields, the computation rapidly takes longer!"
            READ *,nvides
            call GenererGrillePleine(grille)
            print *,"Below, a filled grid:"
            call Afficher_grille(grille)
            ! grid validation:
            if (GrilleValide(grille)) then
                print *, "The grid is valid."
            else
                print *, "The grid is invalid: problem to compute a solution!"
            end if

            Debut = Temps()
            call GenererGrilleSudoku(grille,nvides)
            print *,"Below a Sudoku grid (assuming a likely unique solution):"
            call Afficher_grille(grille)
            if (GrilleValide(grille)) then
                print *, "valid grid"
            else
                print *, "Invalid grid: problem to compute a solution!"
            end if
            Fin = Temps()
            print *,"computing time:", Fin-Debut, "s"
        case(9)
            stop
        end select
    end do

    case (1) ! accessible only by direct invocation of the executable
    call get_command_argument(1, fichier)
    call solver(grille, fichier)

    case default
    print *, "Parameters: enter either one, or none."

    end select
end program main

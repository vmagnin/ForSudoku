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
! Last modifications: 2023-08-31
!------------------------------------------------------------------------------

module sudoku
    use iso_fortran_env, only: dp => real64
    implicit none

contains

    subroutine ResoudreGrille(grille)
        ! input/output parameters:
        integer, dimension(9, 9), intent(inout) :: grille
        ! local variables:
        integer, dimension(9, 9) :: g0        !
        real(kind=dp)     :: alea        ! random number
        integer  :: ligne,colonne,l0,c0,i,j
        integer  :: compteurCV    ! counter of empty/non allocated cells
        integer, dimension(1:81,1:3) :: casesVides    ! list of empty cells
        !logical, dimension(0:9)     :: possible    ! Possibility of each number
        integer, dimension(1:9)      :: chiffrePossible    ! list of (still) possible numbers
        integer :: compteurCP    ! counter of possible numbers

        chiffrePossible = 0

        ! save the initial grid:
        g0 = grille

        ! identify the grid coordinates of empty cells in the grid
        ! in a table of 81 entries
        casesVides = 0
        compteurCV = 0
        do ligne = 1,9
            do colonne =1,9
                if (grille(ligne,colonne) == 0) then
                    compteurCV = compteurCV+1
                    casesVides(compteurCV,1) = ligne
                    casesVides(compteurCV,2) = colonne
                    !call lister_chiffres_possibles(grille,ligne,colonne,casesVides(compteurCV,3),chiffrePossible)
                end if
            end do
        end do

        ! sort the empty cells:
        !call Trier(casesVides,1,compteurCV)

        ! iterate over all empty cells:
        i = 1
        do while (i <= compteurCV)
            ! To accelerate the algorithm, count for each empty cell the numbers
            ! which yet possibly could be inserted
            ! in this very cell
            do j = i,compteurCV
                l0 = casesVides(j,1)
                c0 = casesVides(j,2)
                call lister_chiffres_possibles(grille,l0,c0,casesVides(j,3),chiffrePossible)
            end do
            ! retrieve the empty cells (which depends on the number of still
            ! possible numbers)
            call Trier(casesVides,i,compteurCV)

            ! for cell (l0,c0), generate a list of possible numbers:
            l0 = casesVides(i,1)
            c0 = casesVides(i,2)

            call lister_chiffres_possibles(grille,l0,c0,compteurCP,chiffrePossible)

            ! if there are multiple possibilities, choose one (by chance) and
            ! continue with the next empty cell:
            if (compteurCP > 1) then
                call Random_number(alea)
                j = 1+int(alea*compteurCP)
                grille(l0,c0) = chiffrePossible(j)
                i = i+1
            ! if there is only one possibility, use this number now, and then
            ! continue with the next empty cell
            else if (compteurCP == 1) then
                grille(l0,c0) = chiffrePossible(1)
                i = i+1
            ! start all over again if there is none:
            else
                i = 1
                grille = g0
            end if
        end do
    end subroutine ResoudreGrille


    ! procedure to create a list of allowed numbers in the present empty cell:
    subroutine lister_chiffres_possibles(grille,l0,c0,compteurCP,chiffrePossible)
        ! input parameters:
        integer, dimension(9, 9), intent(in) :: grille
        integer :: l0,c0
        ! output parameters:
        integer, dimension(1:9), intent(out) :: chiffrePossible    ! list of possible numbers
        integer, intent(out) :: compteurCP        ! counter of possible numbers
        ! locale variables:
        integer :: ligne,colonne,cr,lr,j
        logical, dimension(0:9) :: possible    ! Possibility of each number

        possible = .true.
        do j = 1,9
            possible(grille(j,c0)) = .false.
            possible(grille(l0,j)) = .false.
        end do

        lr = 1+3*((l0-1)/3)
        cr = 1+3*((c0-1)/3)
        do ligne = lr,lr+2
            do colonne =cr,cr+2
                possible(grille(ligne,colonne)) = .false.
            end do
        end do

        compteurCP = 0
        chiffrePossible = 0
        do j = 1,9
            if (possible(j)) then
                compteurCP = compteurCP+1
                chiffrePossible(compteurCP) = j
            end if
        end do
    end subroutine

    !****************************************************************
    ! Starting from position p, sort the (still) empty cells by
    ! increasing number of allowed numbers to them.  This is organized
    ! as a bubble sort.
    !****************************************************************
    subroutine Trier(casesVides,p,n)
        ! input parameters:
        integer,intent(in) :: n    ! number of empty lists
        integer,intent(in) :: p    ! sort, start by position p (p inclusive)
        ! output parameters:
        integer, dimension(1:81,1:3), intent(inout)    :: casesVides    ! list of empty cells
        ! local variables:
        integer :: i    ! loop counters
        integer :: j
        integer, dimension(1:3) :: colonne    ! save
        logical :: fini

        fini = .false.
        do while (.not.fini)
            fini = .true.
            ! let's compare each cell with its succeeding cell
            do i = p, n-1
                j = i+1
                if (casesVides(i,3) > casesVides(j,3)) then
                    ! exchange the two cases of this list:
                    colonne =casesVides(i,:)
                    casesVides(i,:) = casesVides(j,:)
                    casesVides(j,:) = colonne
                    fini = .false.
                end if
            end do
        end do
    end subroutine


    ! Grid generation: in each cycle a number is added and the grid is checked
    ! for validity.  If the grid became invalid, the grid generation is starts
    ! all over again.
    ! With a  PIII 866 MHz: about 0.5 s.
    subroutine GenererGrillePleine(grille)
        ! output parameter:
        integer, dimension(9, 9), intent(out) :: grille
        ! local variables:
        real(kind=dp)    :: alea
        integer :: ligne,colonne
        integer(4) :: essais
        logical :: fini

        grille = 0

        ligne = 1
        do while(ligne <= 9)
            colonne =1
            do while(colonne <= 9)
                essais = 0
                fini = .false.
                do while(.not.fini)
                    if (essais > 30) then
                        ! start from the very beginning
                        ! (it were impossible to determine how many cycles one
                        ! has to rewind to identify the erroneous one)
                        grille = 0
                        essais = 0
                        colonne =1
                        ligne = 1
                    end if
                    essais = essais+1
                    call Random_number(alea)
                    grille(ligne,colonne) = 1+int(alea*9_dp)
                    fini = ChiffreValide(grille,ligne,colonne)
                end do
                colonne =colonne+1
            end do
            ligne = ligne+1
        end do
    end subroutine GenererGrillePleine


    logical function ChiffreValide(grille,ligne,colonne)
        ! input:
        integer, dimension(9, 9), intent(in) :: grille
        integer :: ligne,colonne
        ! local variables
        integer :: i,j

        i = (ligne-1)/3
        j = (colonne-1)/3

        ChiffreValide = ColonneOuLigneValide(grille(ligne,1:9)).and.ColonneOuLigneValide(grille(1:9,colonne)) &
            & .and.RegionValide(grille(i*3+1:i*3+3,j*3+1:j*3+3))
    end function ChiffreValide


    ! Note: at present it is unknown if there are Sudoku grids with less than
    ! 17 non-zero cells leading to a unique solution.
    subroutine GenererGrilleSudoku(grille,restant)
        ! output parameter:
        integer, dimension(9, 9), intent(inout) :: grille
        ! input parameter:
        integer,intent(in) :: restant
        ! local variables:
        integer, parameter                :: n = 10
        integer, dimension(9, 9)      :: g0
        integer, dimension(1:n, 1:9, 1:9) :: solutions
        real(kind=dp)    :: alea
        integer :: ligne,colonne,i
        logical :: vide,unique

        ! save the initial grid:
        g0 = grille

        unique = .false.
        do while(.not.unique)
            grille = g0

            ! remove randomly empty cells
            do i = 1, 81-restant
                    ! by chance, one picks a of the cells to be removed:
                    vide = .false.
                    do while(.not.vide)
                        call Random_number(alea)
                        ligne = 1+int(alea*9_dp)
                        call Random_number(alea)
                        colonne =1+int(alea*9_dp)
                        if (grille(ligne,colonne) /= 0) then
                            vide = .true.
                        end if
                    end do
                    ! erase the previously assigned number in this cell:
                    grille(ligne,colonne) = 0
            end do

            print *,"Search of a grid with unique solution ..."

            ! the grid is solved n times
            unique = .true.
            i = 1
    sol :        do while((i <= n).and.unique)
                solutions(i,1:9,1:9) = grille
                call ResoudreGrille(solutions(i,1:9,1:9))
                if (i >= 2) then
                    do ligne = 1,9
                        do colonne =1,9
                            if (solutions(i,ligne,colonne) /= solutions(i-1,ligne,colonne)) then
                                unique = .false.
                                EXIT sol
                            end if
                        end do
                    end do
                end if
                i = i+1
            end do sol

            ! With n identical solutions, one likely identified the wanted
            ! unique solution.  Else, warn the user.
        end do
    end subroutine GenererGrilleSudoku


    subroutine Enregistrer_grille(grille, nom_fichier)
        integer, dimension(9, 9) :: grille
        character(len=*) :: nom_fichier
        ! local variables
        integer :: ligne,colonne    ! line numbers and column numbers
        integer :: fileunit, error
        ! file creation:
        open(newunit=fileunit, file=nom_fichier, STATUS="REPLACE")

        do ligne = 1, 9
            write(fileunit,'(3i2, " |", 3i2, " |", 3i2)') (grille(ligne,colonne) , colonne=1,9)
            if ((ligne == 3).or.(ligne == 6)) then
                write(fileunit,*) "------+-------+------"
            end if
        end do

        close(fileunit)
    end subroutine Enregistrer_grille


    subroutine Lire_grille(grille, nom_fichier)
        ! output parameter:
        integer, dimension(9, 9), intent(out) :: grille
        ! input parameter:
        character(len=*) :: nom_fichier
        ! local variables:
        character(len=2) :: barre1,barre2   ! to read the pipe/the vertical bar
        integer          :: ligne    ! line
        integer :: fileunit, error
        logical :: file_exists  ! check for the presence of the file requested

        inquire(file = nom_fichier, exist = file_exists)
        if (file_exists .eqv. .False.) stop "The requested file is absent."

        ! open and read the file, line by line
        open(newunit=fileunit, file=nom_fichier)

        do ligne = 1, 9
            READ(fileunit,'(3i2, a2, 3i2, a2, 3i2)') &
                grille(ligne,1:3), barre1, grille(ligne,4:6), barre2, grille(ligne,7:9)

            ! skip the lines of dashes
            if ((ligne == 3).or.(ligne == 6)) then
                READ(fileunit,*)
            end if
        end do

        close(fileunit)
    end subroutine Lire_grille


    subroutine Afficher_grille(grille)
        integer, dimension(9, 9) :: grille
        integer :: ligne,c    ! line numbers and column numbers

        do ligne = 1, 9
            print '(3i2, " |", 3i2, " |", 3i2)', (grille(ligne,c) , c=1,9)
            if ((ligne == 3).or.(ligne == 6)) then
                print *, "------+-------+------"
            end if
        end do
    end subroutine Afficher_grille


    subroutine Demander_grille(grille)
        ! input/output:
        integer, dimension(9, 9), intent(inout) :: grille
        ! local variables:
        integer :: ligne,c    ! line numbers and column numbers

        do ligne = 1, 9
            write (*, "(A, I1, A)") "Enter line ", ligne, ":"
            READ *, (grille(ligne,c) , c=1,9)
        end do
    end subroutine Demander_grille


    logical function ColonneOuLigneValide(col)
        ! input parameter:
        integer, dimension(1:9) :: col
        ! local variables:
        integer, dimension(0:9) :: compteur    ! count the occurrence of each number
        integer :: ligne        ! loop counter

        ColonneOuLigneValide = .true.
        compteur = 0
        do ligne = 1,9
            compteur(col(ligne)) = compteur(col(ligne))+1
            if ((compteur(col(ligne)) > 1).and.(col(ligne) /= 0)) then
                ColonneOuLigneValide = .false.
                return        ! leave the function
            end if
        end do
    end function ColonneOuLigneValide


    logical function RegionValide(region)
        ! input:
        integer, dimension(1:3, 1:3) :: region
        integer, dimension(1:9)      :: col

        col(1) = region(1,1)
        col(2) = region(1,2)
        col(3) = region(1,3)
        col(4) = region(2,1)
        col(5) = region(2,2)
        col(6) = region(2,3)
        col(7) = region(3,1)
        col(8) = region(3,2)
        col(9) = region(3,3)
        if (ColonneOuLigneValide(col)) then
            RegionValide = .true.
        else
            RegionValide = .false.
        end if
    end function RegionValide


    logical function GrilleValide(grille)
        ! input:
        integer, dimension(9, 9) :: grille
        ! local variables:
        integer :: ligne,colonne

        GrilleValide = .true.

        ! verification of lines:
        do ligne = 1,9
            if (.not.ColonneOuLigneValide(grille(ligne,1:9))) then
                GrilleValide = .false.
                return
                !print *, "Line ",ligne," is not a valid input"
            end if
        end do

        ! verification of columns:
        do colonne =1,9
            if (.not.ColonneOuLigneValide(grille(1:9,colonne))) then
                GrilleValide = .false.
                return
                !print *, "Column ",colonne," is not a valid input"
            end if
        end do

        ! verification of regions:
        do ligne = 1,7,+3
            do colonne =1,7,+3
                if (.not.RegionValide(grille(ligne:ligne+2,colonne:colonne+2))) then
                    GrilleValide = .false.
                    return
                    !print *, "Region ",ligne,colonne," is not a valid input"
                end if
            end do
        end do
    end function GrilleValide

    !************************************************************
    ! initialization of a system independent pseudorandom generator
    !************************************************************
    subroutine Initialiser_Random
        integer(4), dimension(1:8) :: valeursTemps
        integer(4), allocatable, dimension (:) :: graine

        integer(4) :: boucle , n

        call date_and_time(VALUES = valeursTemps)

        ! retrieve the integers to store a seed: !? On récupère le nombre d'entiers servant à stocker la graine :
        call random_seed(SIZE = n)
        allocate(graine(1:n))

        ! use thousandths of a second by the clock:
        do boucle = 1 , n
            graine(boucle) = huge(graine(boucle))/1000*valeursTemps(8)
        end do

        ! hand over the seed:
        call random_seed(put = graine(1:n))
    end subroutine Initialiser_Random


    !***********************************************************
    ! return the CPU time (expressed in seconds)
    ! cpu_time() is defined by standards of Fortran 95, and later.
    !***********************************************************
    real(kind=dp) function Temps()
        Real(kind=dp) :: t

        call cpu_time(t)
        Temps = t
    end function Temps

    subroutine solver(grille, fichier)
    ! ******************************************************************
    ! provide a solution for a partially filled grid provided as a file
    !
    ! Concept study for a direct invocation of the executable by the CLI
    ! as, for example, by
    !
    ! ```shell
    ! $ ./executable test_in_02.txt
    ! ```
    !
    ! ******************************************************************
    ! input:
    character(len = 50), intent(in) :: fichier
    integer, dimension(9, 9), intent(inout) :: grille
    ! local variables:
    logical :: presence
    presence = .False.

    inquire(file = fichier, exist = presence)
        if (presence .eqv. .False.) then
            print *, "The requested file '", trim(fichier), "' is inaccessible."
        end if

    call Lire_grille(grille, fichier)

    if (GrilleValide(grille) .eqv. .True.) then
        call ResoudreGrille(grille)
        call Afficher_grille(grille)
    else
        print *, "The input by file'", trim(fichier), "' is an invalid grid."
    end if

    end subroutine solver
end module sudoku

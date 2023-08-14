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

module sudoku
    use iso_fortran_env, only: dp => real64
    implicit none

contains

    subroutine ResoudreGrille(g)
        ! input/output parameters:
        integer, dimension(1:9, 1:9), intent(inout) :: g
        ! local variables:
        integer, dimension(1:9, 1:9) :: g0        ! save g
        real(kind=dp)     :: alea        ! random number
        integer  :: l,c,l0,c0,i,j
        integer  :: compteurCV    ! counter of empty/non allocated cells
        integer, dimension(1:81,1:3) :: casesVides    ! list of empty cells
        !logical, dimension(0:9)     :: possible    ! Possibility of each number
        integer, dimension(1:9)      :: chiffrePossible    ! list of (still) possible numbers
        integer :: compteurCP    ! counter of possible numbers

        chiffrePossible = 0

        ! save the initial grid:
        g0 = g

        ! identify the grid coordinates of empty cells in the grid
        ! in a table of 81 entries
        casesVides = 0
        compteurCV = 0
        do l = 1,9
            do c = 1,9
                if (g(l,c) == 0) then
                    compteurCV = compteurCV+1
                    casesVides(compteurCV,1) = l
                    casesVides(compteurCV,2) = c
                    !call lister_chiffres_possibles(g,l,c,casesVides(compteurCV,3),chiffrePossible)
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
                call lister_chiffres_possibles(g,l0,c0,casesVides(j,3),chiffrePossible)
            end do
            ! retrieve the empty cells (which depends on the number of still
            ! possible numbers)
            call Trier(casesVides,i,compteurCV)

            ! for cell (l0,c0), generate a list of possible numbers:
            l0 = casesVides(i,1)
            c0 = casesVides(i,2)

            call lister_chiffres_possibles(g,l0,c0,compteurCP,chiffrePossible)

            ! if there are multiple possibilities, choose one (by chance) and
            ! continue with the next empty cell:
            if (compteurCP > 1) then
                call Random_number(alea)
                j = 1+int(alea*compteurCP)
                g(l0,c0) = chiffrePossible(j)
                i = i+1
            ! if there is only one possibility, use this number now, and then
            ! continue with the next empty cell
            else if (compteurCP == 1) then
                g(l0,c0) = chiffrePossible(1)
                i = i+1
            ! start all over again if there is none:
            else
                i = 1
                g = g0
            end if
        end do
    end subroutine ResoudreGrille


    ! procedure to create a list of allowed numbers in the present empty cell:
    subroutine lister_chiffres_possibles(g,l0,c0,compteurCP,chiffrePossible)
        ! input parameters:
        integer, dimension(1:9, 1:9), intent(in) :: g
        integer :: l0,c0
        ! output parameters:
        integer, dimension(1:9), intent(out) :: chiffrePossible    ! list of possible numbers
        integer, intent(out) :: compteurCP        ! counter of possible numbers
        ! locale variables:
        integer :: l,c,cr,lr,j
        logical, dimension(0:9) :: possible    ! Possibility of each number

        possible = .true.
        do j = 1,9
            possible(g(j,c0)) = .false.
            possible(g(l0,j)) = .false.
        end do

        lr = 1+3*((l0-1)/3)
        cr = 1+3*((c0-1)/3)
        do l = lr,lr+2
            do c = cr,cr+2
                possible(g(l,c)) = .false.
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
        integer, dimension(1:3) :: c    ! save
        logical :: fini

        fini = .false.
        do while (.not.fini)
            fini = .true.
            ! let's compare each cell with its succeeding cell
            do i = p, n-1
                j = i+1
                if (casesVides(i,3) > casesVides(j,3)) then
                    ! exchange the two cases of this list:
                    c = casesVides(i,:)
                    casesVides(i,:) = casesVides(j,:)
                    casesVides(j,:) = c
                    fini = .false.
                end if
            end do
        end do
    end subroutine


    ! Grid generation: in each cycle a number is added and the grid is checked
    ! for validity.  If the grid became invalid, the grid generation is starts
    ! all over again.
    ! With a  PIII 866 MHz: about 0.5 s.
    subroutine GenererGrillePleine(g)
        ! output parameter:
        integer, dimension(1:9, 1:9), intent(out) :: g
        ! local variables:
        real(kind=dp)    :: alea
        integer :: l,c
        integer(4) :: essais
        logical :: fini

        g = 0

        l = 1
        do while(l <= 9)
            c = 1
            do while(c <= 9)
                essais = 0
                fini = .false.
                do while(.not.fini)
                    if (essais > 30) then
                        ! start from the very beginning
                        ! (it were impossible to determine how many cycles one
                        ! has to rewind to identify the erroneous one)
                        g = 0
                        essais = 0
                        c = 1
                        l = 1
                    end if
                    essais = essais+1
                    call Random_number(alea)
                    g(l,c) = 1+int(alea*9_dp)
                    fini = ChiffreValide(g,l,c)
                end do
                c = c+1
            end do
            l = l+1
        end do
    end subroutine GenererGrillePleine


    logical function ChiffreValide(g,l,c)
        ! input:
        integer, dimension(1:9, 1:9), intent(in) :: g
        integer :: l,c
        ! local variables
        integer :: i,j

        i = (l-1)/3
        j = (c-1)/3

        ChiffreValide = ColonneOuLigneValide(g(l,1:9)).and.ColonneOuLigneValide(g(1:9,c)) &
            & .and.RegionValide(g(i*3+1:i*3+3,j*3+1:j*3+3))
    end function ChiffreValide


    ! Note: at present it is unknown if there are Sudoku grids with less than
    ! 17 non-zero cells leading to a unique solution.
    subroutine GenererGrilleSudoku(g,restant)
        ! output parameter:
        integer, dimension(1:9, 1:9), intent(inout) :: g
        ! input parameter:
        integer,intent(in) :: restant
        ! local variables:
        integer, parameter                :: n = 10
        integer, dimension(1:9, 1:9)      :: g0
        integer, dimension(1:n, 1:9, 1:9) :: solutions
        real(kind=dp)    :: alea
        integer :: l,c,i
        logical :: vide,unique

        ! save the initial grid:
        g0 = g

        unique = .false.
        do while(.not.unique)
            g = g0

            ! remove randomly empty cells
            do i = 1, 81-restant
                    ! by chance, one picks a of the cells to be removed:
                    vide = .false.
                    do while(.not.vide)
                        call Random_number(alea)
                        l = 1+int(alea*9_dp)
                        call Random_number(alea)
                        c = 1+int(alea*9_dp)
                        if (g(l,c) /= 0) then
                            vide = .true.
                        end if
                    end do
                    ! erase the previously assigned number in this cell:
                    g(l,c) = 0
            end do

            print *,"Search of a grid with unique solution ..."

            ! the grid is solved n times
            unique = .true.
            i = 1
    sol :        do while((i <= n).and.unique)
                solutions(i,1:9,1:9) = g
                call ResoudreGrille(solutions(i,1:9,1:9))
                if (i >= 2) then
                    do l = 1,9
                        do c = 1,9
                            if (solutions(i,l,c) /= solutions(i-1,l,c)) then
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


    subroutine Enregistrer_grille(g, nom_fichier)
        integer, dimension(1:9, 1:9) :: g
        character(len=*) :: nom_fichier
        ! local variables
        integer :: l,c    ! line numbers and column numbers
        integer :: fileunit, error
        ! file creation:
        open(newunit=fileunit, file=nom_fichier, STATUS="REPLACE")

        do l = 1, 9
            write(fileunit,'(3i2, " |", 3i2, " |", 3i2)') (g(l,c) , c=1,9)
            if ((l == 3).or.(l == 6)) then
                write(fileunit,*) "------+-------+------"
            end if
        end do

        close(fileunit)
    end subroutine Enregistrer_grille


    subroutine Lire_grille(g, nom_fichier)
        ! output parameter:
        integer, dimension(1:9, 1:9), intent(out) :: g
        ! input parameter:
        character(len=*) :: nom_fichier
        ! local variables:
        character(len=2) :: barre1,barre2   ! to read the pipe/the vertical bar
        integer          :: l    ! line numbers
        integer :: fileunit, error
        logical :: file_exists  ! check for the presence of the file requested

        inquire(file = nom_fichier, exist = file_exists)
        if (file_exists .eqv. .False.) stop "The requested file is absent."

        ! open and read the file, line by line
        open(newunit=fileunit, file=nom_fichier)

        do l = 1, 9
            READ(fileunit,'(3i2, a2, 3i2, a2, 3i2)') &
                & g(l,1),g(l,2),g(l,3), barre1,g(l,4),g(l,5),g(l,6), &
                & barre2,g(l,7),g(l,8),g(l,9)

            ! skip the lines of dashes
            if ((l == 3).or.(l == 6)) then
                READ(fileunit,*)
            end if
        end do

        close(fileunit)
    end subroutine Lire_grille


    subroutine Afficher_grille(g)
        integer, dimension(1:9, 1:9) :: g
        integer :: l,c    ! line numbers and column numbers

        do l = 1, 9
            print '(3i2, " |", 3i2, " |", 3i2)', (g(l,c) , c=1,9)
            if ((l == 3).or.(l == 6)) then
                print *, "------+-------+------"
            end if
        end do
    end subroutine Afficher_grille


    subroutine Demander_grille(g)
        ! input/output:
        integer, dimension(1:9, 1:9), intent(inout) :: g
        ! local variables:
        integer :: l,c    ! line numbers and column numbers

        do l = 1, 9
            write (*, "(A, I1, A)") "Enter line ", l, ":"
            READ *, (g(l,c) , c=1,9)
        end do
    end subroutine Demander_grille


    logical function ColonneOuLigneValide(col)
        ! input parameter:
        integer, dimension(1:9) :: col
        ! local variables:
        integer, dimension(0:9) :: compteur    ! count the occurrence of each number
        integer :: l        ! loop counter

        ColonneOuLigneValide = .true.
        compteur = 0
        do l = 1,9
            compteur(col(l)) = compteur(col(l))+1
            if ((compteur(col(l)) > 1).and.(col(l) /= 0)) then
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


    logical function GrilleValide(g)
        ! input:
        integer, dimension(1:9, 1:9) :: g
        ! local variables:
        integer :: l,c

        GrilleValide = .true.

        ! verification of lines:
        do l = 1,9
            if (.not.ColonneOuLigneValide(g(l,1:9))) then
                GrilleValide = .false.
                return
                !print *, "Line ",l," is not a valid input"
            end if
        end do

        ! verification of columns:
        do c = 1,9
            if (.not.ColonneOuLigneValide(g(1:9,c))) then
                GrilleValide = .false.
                return
                !print *, "Column ",c," is not a valid input"
            end if
        end do

        ! verification of regions:
        do l = 1,7,+3
            do c = 1,7,+3
                if (.not.RegionValide(g(l:l+2,c:c+2))) then
                    GrilleValide = .false.
                    return
                    !print *, "Region ",l,c," is not a valid input"
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

    subroutine solver(g, fichier)
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
    integer, dimension(1:9, 1:9), intent(inout) :: g
    ! local variables:
    logical :: presence
    presence = .False.

    inquire(file = fichier, exist = presence)
        if (presence .eqv. .False.) then
            print *, "The requested file '", trim(fichier), "' is inaccessible."
        end if

    call Lire_grille(g, fichier)

    if (GrilleValide(g) .eqv. .True.) then
        call ResoudreGrille(g)
        call Afficher_grille(g)
    else
        print *, "The input by file'", trim(fichier), "' is an invalid grid."
    end if

    end subroutine solver
end module sudoku

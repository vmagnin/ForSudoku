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
! Contributed by Vincent Magnin, 2006-11-27
! Last modifications: 2023-07-22
!------------------------------------------------------------------------------

module sudoku
    use iso_fortran_env, only: dp => real64
    implicit none

contains

    subroutine ResoudreGrille(g)
        ! Parametre d'entrée/sortie :
        integer, dimension(1:9, 1:9), intent(inout) :: g
        ! Variables locales
        integer, dimension(1:9, 1:9) :: g0        ! Sauvegarde de g
        real(kind=dp)     :: alea        ! Nombre aléatoire
        integer  :: l,c,l0,c0,i,j
        integer  :: compteurCV    ! Compteur de cases vides
        integer, dimension(1:81,1:3) :: casesVides    ! Liste des cases vides
        !logical, dimension(0:9)     :: possible    ! Possibilité de chaque chiffre
        integer, dimension(1:9)      :: chiffrePossible    ! Liste des chiffres possibles
        integer :: compteurCP    ! Compteur de chiffres possibles

        chiffrePossible = 0

        ! Sauvegarde de la grille de départ :
        g0 = g

        ! Parcourir la grille, repérer les cases vides et stocker leurs coordonnées
        ! dans un tableau de 81 éléments.
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

        ! Trier les cases vides  :
        !call Trier(casesVides,1,compteurCV)

        ! Parcourir l'ensemble des cases vides.
        i = 1
        do while (i <= compteurCV)
            ! Afin d'accélèrer l'algorithme, a chaque fois,
            ! on recompte le nombre de chiffres possibles pour
            ! chaque case vide restante :
            do j = i,compteurCV
                l0 = casesVides(j,1)
                c0 = casesVides(j,2)
                call lister_chiffres_possibles(g,l0,c0,casesVides(j,3),chiffrePossible)
            end do
            ! Et on retrie les cases vides en fonction du nombre de chiffres possibles
            call Trier(casesVides,i,compteurCV)

            ! Etablir la liste des chiffres possibles dans la case vide l0,c0 :
            l0 = casesVides(i,1)
            c0 = casesVides(i,2)

            call lister_chiffres_possibles(g,l0,c0,compteurCP,chiffrePossible)

            ! S'il y en a plusieurs, en tirer un au hasard et passer à la case vide suivante :
            if (compteurCP > 1) then
                call Random_number(alea)
                j = 1+int(alea*compteurCP)
                g(l0,c0) = chiffrePossible(j)
                i = i+1
            ! S'il n'y en a qu'un, le choisir et passer à la case vide suivante :
            else if (compteurCP == 1) then
                g(l0,c0) = chiffrePossible(1)
                i = i+1
            ! S'il n'y en a pas, on recommence tout :
            else
                i = 1
                g = g0
            end if
        end do
    end subroutine ResoudreGrille


    ! Procédure établissant la liste des chiffres possibles dans une case vide :
    subroutine lister_chiffres_possibles(g,l0,c0,compteurCP,chiffrePossible)
        ! Parametres d'entrée :
        integer, dimension(1:9, 1:9), intent(in) :: g
        integer :: l0,c0
        ! Parametres de sortie :
        integer, dimension(1:9), intent(out) :: chiffrePossible    ! Liste des chiffres possibles
        integer, intent(out) :: compteurCP        ! Compteur de chiffres possibles
        ! Variables locales
        integer :: l,c,cr,lr,j
        logical, dimension(0:9) :: possible    ! Possibilité de chaque chiffre

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
    ! Permet de trier les cases vides par ordre croissant en fonction
    ! du nombre de chiffres possibles, à partir de la position p.
    ! Tri à bulle.
    !****************************************************************
    subroutine Trier(casesVides,p,n)
        ! Parametre d'entree :
        integer,intent(in) :: n    ! Nombre de cases vides
        integer,intent(in) :: p    ! On trie a partir de la position p
        ! Parametre de sortie :
        integer, dimension(1:81,1:3), intent(inout)    :: casesVides    ! Liste des cases vides
        ! Variables locales :
        integer :: i    ! Compteurs de boucle
        integer :: j
        integer, dimension(1:3) :: c    ! Sauvegarde
        logical :: fini

        fini = .false.
        do while (.not.fini)
            fini = .true.
            ! On compare chaque case avec la suivante :
            do i = p, n-1
                j = i+1
                if (casesVides(i,3) > casesVides(j,3)) then
                    ! On echange les deux cases dans la liste :
                    c = casesVides(i,:)
                    casesVides(i,:) = casesVides(j,:)
                    casesVides(j,:) = c
                    fini = .false.
                end if
            end do
        end do
    end subroutine


    ! Génération de grilles : on ajoute un chiffre à la fois, on vérifie la validité,
    ! et on recommence tout si l'on est coincé.
    ! Sur PIII 866 MHZ : environ 0,5 seconde.
    subroutine GenererGrillePleine(g)
        ! Parametre de sortie :
        integer, dimension(1:9, 1:9), intent(out) :: g
        ! Variables locales
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
                        ! On recommence depuis le départ
                        ! (on ne sait pas jusqu'à revenir en arrière)
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
        ! Entrée :
        integer, dimension(1:9, 1:9), intent(in) :: g
        integer :: l,c
        ! Variables locales
        integer :: i,j

        i = (l-1)/3
        j = (c-1)/3

        ChiffreValide = ColonneOuLigneValide(g(l,1:9)).and.ColonneOuLigneValide(g(1:9,c)) &
            & .and.RegionValide(g(i*3+1:i*3+3,j*3+1:j*3+3))
    end function ChiffreValide


    ! Remarque : on ne sait pas pour l'instant s'il existe des grilles de sudoku
    ! contenant moins de 17 chiffres tout en ayant une solution unique.
    subroutine GenererGrilleSudoku(g,restant)
        ! Parametre de sortie :
        integer, dimension(1:9, 1:9), intent(inout) :: g
        ! Parametre d'entree :
        integer,intent(in) :: restant
        ! Variables locales
        integer, parameter                :: n = 10
        integer, dimension(1:9, 1:9)      :: g0
        integer, dimension(1:n, 1:9, 1:9) :: solutions
        real(kind=dp)    :: alea
        integer :: l,c,i
        logical :: vide,unique

        ! Sauvegarde de la grille de départ :
        g0 = g

        unique = .false.
        do while(.not.unique)
            g = g0

            ! On enlève au hasard les cases vides
            do i = 1, 81-restant
                    ! On cherche une case à effacer :
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
                    ! Et on la vide :
                    g(l,c) = 0
            end do

            print *,"Je cherche une grille avec une solution unique..."

            ! On résout n fois la grille
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

            ! Si les n solutions sont identiques, la solution est
            ! probablement unique. Sinon on avertit l'utilisateur.
        end do
    end subroutine GenererGrilleSudoku


    subroutine Enregistrer_grille(g, nom_fichier)
        integer, dimension(1:9, 1:9) :: g
        character(len=*) :: nom_fichier
        ! Variables locales :
        integer :: l,c    !Numéros lignes et colonnes
        integer :: fileunit, error
        ! Creation du fichier :
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
        ! Parametre de sortie :
        integer, dimension(1:9, 1:9), intent(out) :: g
        ! Parametre d'entree :
        character(len=*) :: nom_fichier
        ! Variables locales
        character(len=2) :: barre1,barre2   !Pour lire les barres
        integer       :: l    !Numeros lignes
        integer :: fileunit, error
        logical :: file_exists  ! check for the presence of the file requested

        inquire(file = nom_fichier, exist = file_exists)
        if (file_exists .eqv. .False.) stop "The requested file is absent."

        ! Ouverture et lecture du fichier ligne par ligne :
        open(newunit=fileunit, file=nom_fichier)

        do l = 1, 9
            READ(fileunit,'(3i2, a2, 3i2, a2, 3i2)') &
                & g(l,1),g(l,2),g(l,3), barre1,g(l,4),g(l,5),g(l,6), &
                & barre2,g(l,7),g(l,8),g(l,9)

            ! On saute les lignes de tirets :
            if ((l == 3).or.(l == 6)) then
                READ(fileunit,*)
            end if
        end do

        close(fileunit)
    end subroutine Lire_grille


    subroutine Afficher_grille(g)
        integer, dimension(1:9, 1:9) :: g
        integer :: l,c    !Numeros lignes et colonnes

        do l = 1, 9
            print '(3i2, " |", 3i2, " |", 3i2)', (g(l,c) , c=1,9)
            if ((l == 3).or.(l == 6)) then
                print *, "------+-------+------"
            end if
        end do
    end subroutine Afficher_grille


    subroutine Demander_grille(g)
        ! Entree-sortie :
        integer, dimension(1:9, 1:9), intent(inout) :: g
        ! Variables locales :
        integer :: l,c    !Numeros lignes et colonnes

        do l = 1, 9
            print *, "Entrez la ligne ",l
            READ *, (g(l,c) , c=1,9)
        end do
    end subroutine Demander_grille


    logical function ColonneOuLigneValide(col)
        ! ParamÃštre d'entrée :
        integer, dimension(1:9) :: col
        ! Variables locales :
        integer, dimension(0:9) :: compteur    !Nb d'apparitions de chaque chiffre
        integer :: l        !Compteur de boucle

        ColonneOuLigneValide = .true.
        compteur = 0
        do l = 1,9
            compteur(col(l)) = compteur(col(l))+1
            if ((compteur(col(l)) > 1).and.(col(l) /= 0)) then
                ColonneOuLigneValide = .false.
                return        !On quitte la fonction
            end if
        end do
    end function ColonneOuLigneValide


    logical function RegionValide(region)
        ! Entrée :
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
        ! Entrée :
        integer, dimension(1:9, 1:9) :: g
        ! Variables locales :
        integer :: l,c

        GrilleValide = .true.

        ! Vérification des lignes :
        do l = 1,9
            if (.not.ColonneOuLigneValide(g(l,1:9))) then
                GrilleValide = .false.
                return
                !print *, "Ligne ",l," non valide"
            end if
        end do

        ! Vérification des colonnes :
        do c = 1,9
            if (.not.ColonneOuLigneValide(g(1:9,c))) then
                GrilleValide = .false.
                return
                !print *, "Colonne ",c," non valide"
            end if
        end do

        ! Vérification des régions :
        do l = 1,7,+3
            do c = 1,7,+3
                if (.not.RegionValide(g(l:l+2,c:c+2))) then
                    GrilleValide = .false.
                    return
                    !print *, "Region ",l,c," non valide"
                end if
            end do
        end do
    end function GrilleValide

    !************************************************************
    ! Initialisation du générateur de nombres pseudo-aléatoires
    ! indépendamment du système
    !************************************************************
    subroutine Initialiser_Random
        integer(4), dimension(1:8) :: valeursTemps
        integer(4), allocatable, dimension (:) :: graine

        integer(4) :: boucle , n

        call date_and_time(VALUES = valeursTemps)

        ! On récupère le nombre d'entiers servant à stocker la graine :
        call random_seed(SIZE = n)
        allocate(graine(1:n))

        ! On utilise les millièmes de seconde de l'horloge :
        do boucle = 1 , n
            graine(boucle) = huge(graine(boucle))/1000*valeursTemps(8)
        end do

        ! On transmet la graine :
        call random_seed(put = graine(1:n))
    end subroutine Initialiser_Random


    !***********************************************************
    !   Retourne le temps CPU en secondes.
    !    cpu_time() est définie dans la norme Fortran 95.
    !***********************************************************
    real(kind=dp) function Temps()
        Real(kind=dp) :: t

        call cpu_time(t)
        Temps = t
    end function Temps

end module sudoku

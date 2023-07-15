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
! Last modifications: 2023-07-15
!------------------------------------------------------------------------------

program main
    use sudoku

    implicit none
    ! Variables locales :
    integer(1), dimension(1:9, 1:9) :: grille    ! (ligne,colonne)
    real(8)       :: Debut,Fin    ! Pour mesurer la durée de calcul
    integer(1)    :: choix
    integer(1)    :: nvides    ! Nombre de cases à vider
    character(50) :: fichier    ! Nom du fichier .txt

    ! Initialisation du générateur de nombres pseudo-aléatoires :
    call Initialiser_Random
    ! Initialisation de la grille avec des cases vides (codées par des 0) :
    grille = 0

    print *,"sudoku.f90, version 0.8, copyright (C) 2006 Vincent MAGNIN"
    ! Boucle infinie du menu :
    do
        print *
        print *,"*************************** MENU ********************************"
        print *,"1) Taper une grille."
        print *,"2) Lire une grille dans un fichier."
        print *,"3) Enregistrer une grille dans un fichier."
        print *,"4) Verifier la validite de la grille en memoire."
        print *,"5) Afficher la grille en memoire."
        print *,"6) Generer une grille pleine."
        print *,"7) Resoudre la grille en memoire."
        print *,"8) Generer une grille admettant une solution probablement unique."
        print *,"9) Quitter."
        print *,"Tapez un de ces numeros et appuyez sur 'Entree' :"
        READ *,choix

        select case (choix)
        case (1)
            call Demander_grille(grille)
            print *,"Vous avez rentre la grille suivante :"
            call Afficher_grille(grille)
            if (GrilleValide(grille)) then
                print *, "Elle est valide."
            else
                print *, "Elle n'est pas valide."
            end if
        case(2)
            print *,"Entrez le nom du fichier à lire, avec son extension .txt :"
            call SYSTEM("dir *.txt")
            READ *,fichier
            call Lire_grille(grille,trim(fichier))
            call Afficher_grille(grille)
            if (GrilleValide(grille)) then
                print *, "Elle est valide."
            else
                print *, "Elle n'est pas valide."
            end if
        case(3)
            print *,"Entrez le nom du fichier à enregistrer, avec son extension :"
            READ *,fichier
            call Enregistrer_grille(grille,trim(fichier))
            print *,"Enregistrement effectué."
        case(4)
            if (GrilleValide(grille)) then
                print *, "La grille en memoire est valide."
            else
                print *, "La grille en memoire n'est pas valide."
            end if
        case(5)
            print *,"Voici la grille en memoire :"
            call Afficher_grille(grille)
            if (GrilleValide(grille)) then
                print *, "Elle est valide."
            else
                print *, "Elle n'est pas valide."
            end if
        case(6)
            Debut = Temps()
            call GenererGrillePleine(grille)
            call Afficher_grille(grille)
            ! Vérification par sécurité :
            if (GrilleValide(grille)) then
                print *, "Grille valide"
            else
                print *, "Grille non valide : probleme de generation !"
            end if
            Fin = Temps()
            print *,"Temps de calcul :", Fin-Debut, "s"
        case(7)
            print *,"Voici la grille de depart :"
            call Afficher_grille(grille)
            Debut = Temps()
            call ResoudreGrille(grille)
            if (GrilleValide(grille)) then
                print *, "Voici la grille resolue (validite verifiee) :"
            else
                print *, "Grille non valide : probleme de resolution..."
            end if
            call Afficher_grille(grille)
            Fin = Temps()
            print *,"Temps de calcul :", Fin-Debut, "s"
        case(8)
            print *,"Nombre de chiffres dans la grille [17,81] ?"
            print *,"Attention, en dessous de 35, la duree de calcul s'allonge rapidement !"
            READ *,nvides
            call GenererGrillePleine(grille)
            print *,"Voici la grille pleine :"
            call Afficher_grille(grille)
            ! Vérification par sécurité :
            if (GrilleValide(grille)) then
                print *, "Grille valide"
            else
                print *, "Grille non valide : probleme de generation !"
            end if

            Debut = Temps()
            call GenererGrilleSudoku(grille,nvides)
            print *,"Voici une grille de sudoku admettant probablement une solution unique :"
            call Afficher_grille(grille)
            if (GrilleValide(grille)) then
                print *, "Grille valide"
            else
                print *, "Grille non valide : probleme de generation"
            end if
            Fin = Temps()
            print *,"Temps de calcul :", Fin-Debut, "s"
        case(9)
            stop
        end select
    end do
end program main

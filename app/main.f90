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
! Last modifications: 2023-07-14
!------------------------------------------------------------------------------

PROGRAM main
    use sudoku

    IMPLICIT NONE
    ! Variables locales :
    INTEGER(1), DIMENSION(1:9, 1:9) :: grille    ! (ligne,colonne)
    REAL(8)                :: Debut,Fin    ! Pour mesurer la durée de calcul
    INTEGER(1)            :: choix
    INTEGER(1)            :: nvides    ! Nombre de cases à vider
    CHARACTER(50)            :: fichier    ! Nom du fichier .txt

    ! Initialisation du générateur de nombres pseudo-aléatoires :
    CALL Initialiser_Random
    ! Initialisation de la grille avec des cases vides (codées par des 0) :
    grille=0

    PRINT *,"sudoku.f90, version 0.8, copyright (C) 2006 Vincent MAGNIN"
    ! Boucle infinie du menu :
    DO
        PRINT *
        PRINT *,"*************************** MENU ********************************"
        PRINT *,"1) Taper une grille."
        PRINT *,"2) Lire une grille dans un fichier."
        PRINT *,"3) Enregistrer une grille dans un fichier."
        PRINT *,"4) Verifier la validite de la grille en memoire."
        PRINT *,"5) Afficher la grille en memoire."
        PRINT *,"6) Generer une grille pleine."
        PRINT *,"7) Resoudre la grille en memoire."
        PRINT *,"8) Generer une grille admettant une solution probablement unique."
        PRINT *,"9) Quitter."
        PRINT *,"Tapez un de ces numeros et appuyez sur 'Entree' :"
        READ *,choix

        SELECT CASE (choix)
        CASE (1)
            CALL Demander_grille(grille)
            PRINT *,"Vous avez rentre la grille suivante :"
            CALL Afficher_grille(grille)
            IF (GrilleValide(grille)) THEN
                PRINT *, "Elle est valide."
            ELSE
                PRINT *, "Elle n'est pas valide."
            END IF
        CASE(2)
            PRINT *,"Entrez le nom du fichier à lire, avec son extension .txt :"
            CALL SYSTEM("dir *.txt")
            READ *,fichier
            CALL Lire_grille(grille,TRIM(fichier))
            CALL Afficher_grille(grille)
            IF (GrilleValide(grille)) THEN
                PRINT *, "Elle est valide."
            ELSE
                PRINT *, "Elle n'est pas valide."
            END IF
        CASE(3)
            PRINT *,"Entrez le nom du fichier à enregistrer, avec son extension :"
            READ *,fichier
            CALL Enregistrer_grille(grille,TRIM(fichier))
            PRINT *,"Enregistrement effectué."
        CASE(4)
            IF (GrilleValide(grille)) THEN
                PRINT *, "La grille en memoire est valide."
            ELSE
                PRINT *, "La grille en memoire n'est pas valide."
            END IF
        CASE(5)
            PRINT *,"Voici la grille en memoire :"
            CALL Afficher_grille(grille)
            IF (GrilleValide(grille)) THEN
                PRINT *, "Elle est valide."
            ELSE
                PRINT *, "Elle n'est pas valide."
            END IF
        CASE(6)
            Debut=Temps()
            CALL GenererGrillePleine(grille)
            CALL Afficher_grille(grille)
            ! Vérification par sécurité :
            IF (GrilleValide(grille)) THEN
                PRINT *, "Grille valide"
            ELSE
                PRINT *, "Grille non valide : probleme de generation !"
            END IF
            Fin=Temps()
            PRINT *,"Temps de calcul :", Fin-Debut, "s"
        CASE(7)
            PRINT *,"Voici la grille de depart :"
            CALL Afficher_grille(grille)
            Debut=Temps()
            CALL ResoudreGrille(grille)
            IF (GrilleValide(grille)) THEN
                PRINT *, "Voici la grille resolue (validite verifiee) :"
            ELSE
                PRINT *, "Grille non valide : probleme de resolution..."
            END IF
            CALL Afficher_grille(grille)
            Fin=Temps()
            PRINT *,"Temps de calcul :", Fin-Debut, "s"
        CASE(8)
            PRINT *,"Nombre de chiffres dans la grille [17,81] ?"
            PRINT *,"Attention, en dessous de 35, la duree de calcul s'allonge rapidement !"
            READ *,nvides
            CALL GenererGrillePleine(grille)
            PRINT *,"Voici la grille pleine :"
            CALL Afficher_grille(grille)
            ! Vérification par sécurité :
            IF (GrilleValide(grille)) THEN
                PRINT *, "Grille valide"
            ELSE
                PRINT *, "Grille non valide : probleme de generation !"
            END IF

            Debut=Temps()
            CALL GenererGrilleSudoku(grille,nvides)
            PRINT *,"Voici une grille de sudoku admettant probablement une solution unique :"
            CALL Afficher_grille(grille)
            IF (GrilleValide(grille)) THEN
                PRINT *, "Grille valide"
            ELSE
                PRINT *, "Grille non valide : probleme de generation"
            END IF
            Fin=Temps()
            PRINT *,"Temps de calcul :", Fin-Debut, "s"
        CASE(9)
            STOP
        END SELECT
    END DO
END PROGRAM main

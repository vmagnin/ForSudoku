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

module sudoku
    implicit none

contains

    SUBROUTINE ResoudreGrille(g)
        IMPLICIT NONE
        ! Parametre d'entrÃ©e/sortie :
        INTEGER(1), DIMENSION(1:9, 1:9), INTENT(INOUT)    :: g
        ! Variables locales
        INTEGER(1), DIMENSION(1:9, 1:9)    :: g0        ! Sauvegarde de g
        REAL(8)                :: alea        ! Nombre alÃ©atoire
        INTEGER(1)            :: l,c,l0,c0,i,j
        INTEGER(1)            :: compteurCV    ! Compteur de cases vides
        INTEGER(1), DIMENSION(1:81,1:3)    :: casesVides    ! Liste des cases vides
        !LOGICAL(1), DIMENSION(0:9)    :: possible    ! PossibilitÃ© de chaque chiffre
        INTEGER(1), DIMENSION(1:9)    :: chiffrePossible    ! Liste des chiffres possibles
        INTEGER(1)            :: compteurCP    ! Compteur de chiffres possibles

        chiffrePossible=0

        ! Sauvegarde de la grille de dÃ©part :
        g0=g

        ! Parcourir la grille, repÃ©rer les cases vides et stocker leurs coordonnÃ©es
        ! dans un tableau de 81 Ã©lÃ©ments.
        casesVides=0
        compteurCV=0
        DO l=1,9,+1
            DO c=1,9,+1
                IF (g(l,c)==0) THEN
                    compteurCV=compteurCV+1
                    casesVides(compteurCV,1)=l
                    casesVides(compteurCV,2)=c
                    !CALL lister_chiffres_possibles(g,l,c,casesVides(compteurCV,3),chiffrePossible)
                END IF
            END DO
        END DO

        ! Trier les cases vides  :
        !CALL Trier(casesVides,1,compteurCV)

        ! Parcourir l'ensemble des cases vides.
        i=1
        DO WHILE (i<=compteurCV)
            ! Afin d'accï¿œï¿œer l'algorithme, a chaque fois,
            ! on recompte le nombre de chiffres possibles pour
            ! chaque case vide restante :
            DO j=i,compteurCV,+1
                l0=casesVides(j,1)
                c0=casesVides(j,2)
                CALL lister_chiffres_possibles(g,l0,c0,casesVides(j,3),chiffrePossible)
            END DO
            ! Et on retrie les cases vides en fonction du nombre de chiffres possibles
            CALL Trier(casesVides,i,compteurCV)

            ! Etablir la liste des chiffres possibles dans la case vide l0,c0 :
            l0=casesVides(i,1)
            c0=casesVides(i,2)

            CALL lister_chiffres_possibles(g,l0,c0,compteurCP,chiffrePossible)

            ! S'il y en a plusieurs, en tirer un au hasard et passer Ã  la case vide suivante :
            IF (compteurCP>1) THEN
                CALL Random_number(alea)
                j=1+INT(alea*compteurCP)
                g(l0,c0)=chiffrePossible(j)
                i=i+1
            ! S'il n'y en a qu'un, le choisir et passer Ã  la case vide suivante :
            ELSE IF (compteurCP==1) THEN
                g(l0,c0)=chiffrePossible(1)
                i=i+1
            ! S'il n'y en a pas, on recommence tout :
            ELSE
                i=1
                g=g0
            END IF
        END DO
    END SUBROUTINE ResoudreGrille


    ! ProcÃ©dure Ã©tablissant la liste des chiffres possibles dans une case vide :
    SUBROUTINE lister_chiffres_possibles(g,l0,c0,compteurCP,chiffrePossible)
        IMPLICIT NONE
        ! Parametres d'entrÃ©e :
        INTEGER(1), DIMENSION(1:9, 1:9), INTENT(IN)    :: g
        INTEGER(1)                    :: l0,c0
        ! Parametres de sortie :
        INTEGER(1), DIMENSION(1:9), INTENT(OUT)    :: chiffrePossible    ! Liste des chiffres possibles
        INTEGER(1), INTENT(OUT)            :: compteurCP        ! Compteur de chiffres possibles
        ! Variables locales
        INTEGER(1)            :: l,c,cr,lr,j
        LOGICAL(1), DIMENSION(0:9)    :: possible    ! PossibilitÃ© de chaque chiffre

        possible=.TRUE.
        DO j=1,9,+1
            possible(g(j,c0))=.FALSE.
            possible(g(l0,j))=.FALSE.
        END DO

        lr=1+3*((l0-1)/3)
        cr=1+3*((c0-1)/3)
        DO l=lr,lr+2,+1
            DO c=cr,cr+2,+1
                possible(g(l,c))=.FALSE.
            END DO
        END DO

        compteurCP=0
        chiffrePossible=0
        DO j=1,9,+1
            IF (possible(j)) THEN
                compteurCP=compteurCP+1
                chiffrePossible(compteurCP)=j
            END IF
        END DO
    END SUBROUTINE

    !****************************************************************
    ! Permet de trier les cases vides par ordre croissant en fonction
    ! du nombre de chiffres possibles, ï¿œpartir de la position p.
    ! Tri ï¿œbulle.
    !****************************************************************
    SUBROUTINE Trier(casesVides,p,n)
        ! Parametre d'entree :
        INTEGER(1),INTENT(IN)        :: n    ! Nombre de cases vides
        INTEGER(1),INTENT(IN)        :: p    ! On trie a partir de la position p
        ! Parametre de sortie :
        INTEGER(1), DIMENSION(1:81,1:3), INTENT(INOUT)    :: casesVides    ! Liste des cases vides
        ! Variables locales :
        INTEGER(1)                        :: i    ! Compteurs de boucle
        INTEGER(1)                        :: j
        INTEGER(1), DIMENSION(1:3)        :: c    ! Sauvegarde
        LOGICAL                            :: fini

        fini=.FALSE.
        DO WHILE (.NOT.fini)
            fini=.TRUE.
            ! On compare chaque case avec la suivante :
            DO i=p, n-1, +1
                j=i+1
                IF (casesVides(i,3)>casesVides(j,3)) THEN
                    ! On echange les deux cases dans la liste :
                    c=casesVides(i,:)
                    casesVides(i,:)=casesVides(j,:)
                    casesVides(j,:)=c
                    fini=.FALSE.
                END IF
            END DO
        END DO
    END SUBROUTINE


    ! GÃ©nÃ©ration de grilles : on ajoute un chiffre Ã  la fois, on vÃ©rifie la validitÃ©,
    ! et on recommence tout si l'on est coincÃ©.
    ! Sur PIII 866 MHZ : environ 0,5 seconde.
    SUBROUTINE GenererGrillePleine(g)
        IMPLICIT NONE
        ! Parametre de sortie :
        INTEGER(1), DIMENSION(1:9, 1:9), INTENT(OUT)    :: g
        ! Variables locales
        REAL(8)                :: alea
        INTEGER(1)            :: l,c
        INTEGER(4)             :: essais
        LOGICAL(1)            :: fini

        g=0

        l=1
        DO WHILE(l<=9)
            c=1
            DO WHILE(c<=9)
                essais=0
                fini=.FALSE.
                DO WHILE(.NOT.fini)
                    IF (essais>30) THEN
                        ! On recommence depuis le dÃ©part
                        ! (on ne sait pas jusqu'oÃ¹ revenir en arriÃšre)
                        g=0
                        essais=0
                        c=1
                        l=1
                    END IF
                    essais=essais+1
                    CALL Random_number(alea)
                    g(l,c)=1+INT(alea*9d0)
                    fini=ChiffreValide(g,l,c)
                END DO
                c=c+1
            END DO
            l=l+1
        END DO
    END SUBROUTINE GenererGrillePleine


    LOGICAL FUNCTION ChiffreValide(g,l,c)
        IMPLICIT NONE
        ! EntrÃ©e :
        INTEGER(1), DIMENSION(1:9, 1:9), INTENT(IN) :: g
        INTEGER(1)            :: l,c
        ! Variables locales
        INTEGER(1)            :: i,j

        i=(l-1)/3
        j=(c-1)/3

        ChiffreValide=ColonneOuLigneValide(g(l,1:9)).AND.ColonneOuLigneValide(g(1:9,c)).AND.RegionValide(g(i*3+1:i*3+3,j*3+1:j*3+3))
    END FUNCTION ChiffreValide


    ! Remarque : on ne sait pas pour l'instant s'il existe des grilles de sudoku
    ! contenant moins de 17 chiffres tout en ayant une solution unique.
    SUBROUTINE GenererGrilleSudoku(g,restant)
        IMPLICIT NONE
        ! Parametre de sortie :
        INTEGER(1), DIMENSION(1:9, 1:9), INTENT(INOUT)    :: g
        ! Parametre d'entree :
        INTEGER(1),INTENT(IN)    :: restant
        ! Variables locales
        INTEGER(1), PARAMETER                    :: n=10
        INTEGER(1), DIMENSION(1:9, 1:9)            :: g0
        INTEGER(1), DIMENSION(1:n, 1:9, 1:9)    :: solutions
        REAL(8)                :: alea
        INTEGER(1)            :: l,c,i
        LOGICAL(1)            :: vide,unique

        ! Sauvegarde de la grille de dÃ©part :
        g0=g

        unique=.FALSE.
        DO WHILE(.NOT.unique)
            g=g0

            ! On enlÃšve au hasard les cases vides
            DO i=1, 81-restant
                    ! On cherche une case Ã  effacer :
                    vide=.FALSE.
                    DO WHILE(.NOT.vide)
                        CALL Random_number(alea)
                        l=1+INT(alea*9d0)
                        CALL Random_number(alea)
                        c=1+INT(alea*9d0)
                        IF (g(l,c)/=0) THEN
                            vide=.TRUE.
                        END IF
                    END DO
                    ! Et on la vide :
                    g(l,c)=0
            END DO

            PRINT *,"Je cherche une grille avec une solution unique..."

            ! On resout n fois la grille
            unique=.TRUE.
            i=1
    sol :        DO WHILE((i<=n).AND.unique)
                solutions(i,1:9,1:9)=g
                CALL ResoudreGrille(solutions(i,1:9,1:9))
                IF (i>=2) THEN
                    DO l=1,9,+1
                        DO c=1,9,+1
                            IF (solutions(i,l,c)/=solutions(i-1,l,c)) THEN
                                unique=.FALSE.
                                EXIT sol
                            END IF
                        END DO
                    END DO
                END IF
                i=i+1
            END DO sol

            ! Si les n solutions sont identiques, la solution est
            ! probablement unique. Sinon on avertit l'utilisateur.
        END DO
    END SUBROUTINE GenererGrilleSudoku


    SUBROUTINE Enregistrer_grille(g, nom_fichier)
        IMPLICIT NONE
        INTEGER(1), DIMENSION(1:9, 1:9) :: g
        CHARACTER(LEN=*)        :: nom_fichier
        ! Variables locales :
        INTEGER(1)            :: l,c    !Numeros lignes et colonnes

        ! Creation du fichier :
        OPEN(UNIT=1, FILE=nom_fichier, STATUS="REPLACE")

        DO l=1, 9, +1
            WRITE(1,'(i2,i2,i2," |",i2,i2,i2," |",i2,i2,i2)') (g(l,c) , c=1,9)
            IF ((l==3).OR.(l==6)) THEN
                WRITE(1,*) "------+-------+------"
            END IF
        END DO

        CLOSE(1)
    END SUBROUTINE Enregistrer_grille


    SUBROUTINE Lire_grille(g, nom_fichier)
        IMPLICIT NONE

        ! Parametre de sortie :
        INTEGER(1), DIMENSION(1:9, 1:9), INTENT(OUT) :: g
        ! Parametre d'entree :
        CHARACTER(LEN=*)    :: nom_fichier
        ! Variables locales
        CHARACTER(LEN=2)    :: barre1,barre2   !Pour lire les barres
        INTEGER(1)        :: l    !Numeros lignes

        ! Ouverture et lecture du fichier ligne par ligne :
        OPEN(UNIT=1, FILE=nom_fichier)

        DO l=1, 9, +1
            READ(1,'(i2,i2,i2,a2,i2,i2,i2,a2,i2,i2,i2)') &
                & g(l,1),g(l,2),g(l,3), barre1,g(l,4),g(l,5),g(l,6), &
                & barre2,g(l,7),g(l,8),g(l,9)

            ! On saute les lignes de tirets :
            IF ((l==3).OR.(l==6)) THEN
                READ(1,*)
            END IF
        END DO

        CLOSE(1)
    END SUBROUTINE Lire_grille


    SUBROUTINE Afficher_grille(g)
        IMPLICIT NONE
        INTEGER(1), DIMENSION(1:9, 1:9) :: g
        INTEGER(1)            :: l,c    !Numeros lignes et colonnes

        DO l=1, 9, +1
            PRINT '(i2,i2,i2," |",i2,i2,i2," |",i2,i2,i2)', (g(l,c) , c=1,9)
            IF ((l==3).OR.(l==6)) THEN
                PRINT *, "------+-------+------"
            END IF
        END DO
    END SUBROUTINE Afficher_grille


    SUBROUTINE Demander_grille(g)
        IMPLICIT NONE
        ! Entree-sortie :
        INTEGER(1), DIMENSION(1:9, 1:9), INTENT(INOUT) :: g
        ! Variables locales :
        INTEGER(1)    :: l,c    !Numeros lignes et colonnes

        DO l=1, 9, +1
            PRINT *, "Entrez la ligne ",l
            READ *, (g(l,c) , c=1,9)
        END DO
    END SUBROUTINE Demander_grille


    LOGICAL FUNCTION ColonneOuLigneValide(col)
        IMPLICIT NONE
        ! ParamÃštre d'entrÃ©e :
        INTEGER(1), DIMENSION(1:9)     :: col
        ! Variables locales :
        INTEGER(1), DIMENSION(0:9)     :: compteur    !Nb d'apparitions de chaque chiffre
        INTEGER(1)            :: l        !Compteur de boucle

        ColonneOuLigneValide=.TRUE.
        compteur=0
        DO l=1,9,+1
            compteur(col(l))=compteur(col(l))+1
            IF ((compteur(col(l))>1).AND.(col(l)/=0)) THEN
                ColonneOuLigneValide=.FALSE.
                RETURN        !On quitte la fonction
            END IF
        END DO
    END FUNCTION ColonneOuLigneValide


    LOGICAL FUNCTION RegionValide(region)
        IMPLICIT NONE
        ! EntrÃ©e :
        INTEGER(1), DIMENSION(1:3, 1:3) :: region
        INTEGER(1), DIMENSION(1:9)     :: col

        col(1)=region(1,1)
        col(2)=region(1,2)
        col(3)=region(1,3)
        col(4)=region(2,1)
        col(5)=region(2,2)
        col(6)=region(2,3)
        col(7)=region(3,1)
        col(8)=region(3,2)
        col(9)=region(3,3)
        IF (ColonneOuLigneValide(col)) THEN
            RegionValide=.TRUE.
        ELSE
            RegionValide=.FALSE.
        END IF
    END FUNCTION RegionValide


    LOGICAL FUNCTION GrilleValide(g)
        IMPLICIT NONE
        ! EntrÃ©e :
        INTEGER(1), DIMENSION(1:9, 1:9) :: g
        ! Variables locales :
        INTEGER(1)            :: l,c

        GrilleValide=.TRUE.

        ! VÃ©rification des lignes :
        DO l=1,9,+1
            IF (.NOT.ColonneOuLigneValide(g(l,1:9))) THEN
                GrilleValide=.FALSE.
                return
                !PRINT *, "Ligne ",l," non valide"
            END IF
        END DO

        ! VÃ©rification des colonnes :
        DO c=1,9,+1
            IF (.NOT.ColonneOuLigneValide(g(1:9,c))) THEN
                GrilleValide=.FALSE.
                RETURN
                !PRINT *, "Colonne ",c," non valide"
            END IF
        END DO

        ! VÃ©rification des rÃ©gions :
        DO l=1,7,+3
            DO c=1,7,+3
                IF (.NOT.RegionValide(g(l:l+2,c:c+2))) THEN
                    GrilleValide=.FALSE.
                    RETURN
                    !PRINT *, "Region ",l,c," non valide"
                END IF
            END DO
        END DO
    END FUNCTION GrilleValide

    !************************************************************
    ! Initialisation du gÃ©nÃ©rateur de nombres pseudo-alÃ©atoires
    ! indÃ©pendamment du systÃšme
    !************************************************************
    SUBROUTINE Initialiser_Random
        IMPLICIT NONE
        INTEGER(4), DIMENSION(1:8) :: valeursTemps
        INTEGER(4), ALLOCATABLE, DIMENSION (:) :: graine

        INTEGER(4) :: boucle , n

        CALL DATE_AND_TIME(VALUES=valeursTemps)

        ! On rÃ©cupÃšre le nombre d'entiers servant Ã  stocker la graine :
        CALL RANDOM_SEED(SIZE=n)
        ALLOCATE(graine(1:n))

        ! On utilise les milliÃšmes de seconde de l'horloge :
        DO boucle=1 , n
            graine(boucle)=HUGE(graine(boucle))/1000*valeursTemps(8)
        END DO

        ! On transmet la graine :
        CALL RANDOM_SEED(put=graine(1:n))
    END SUBROUTINE Initialiser_Random


    !***********************************************************
    !   Retourne le temps CPU en secondes.
    !    cpu_time() est dÃ©finie dans la norme Fortran 95.
    !***********************************************************
    real(8) FUNCTION Temps()
        IMPLICIT NONE
        Real(8) :: t

        call cpu_time(t)
        Temps=t
    END FUNCTION Temps

end module sudoku

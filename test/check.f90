program check
   use sudoku, only: ResoudreGrille
   implicit none

   call assert_wikipedia_solution()

contains

   subroutine assert_wikipedia_solution()
      ! voir : https://en.wikipedia.org/wiki/Sudoku
      ! tous les variables sont locales
      integer :: grille_a(9,9), grille_b(9,9)
      integer :: i, j
      logical :: array_equality

      array_equality = .true.

      ! la grille à résoudre
      grille_a(:, 1) = [5, 3, 0,  0, 7, 0,  0, 0, 0]
      grille_a(:, 2) = [6, 0, 0,  1, 9, 5,  0, 0, 0]
      grille_a(:, 3) = [0, 9, 8,  0, 0, 0,  0, 6, 0]

      grille_a(:, 4) = [8, 0, 0,  0, 6, 0,  0, 0, 3]
      grille_a(:, 5) = [4, 0, 0,  8, 0, 3,  0, 0, 1]
      grille_a(:, 6) = [7, 0, 0,  0, 2, 0,  0, 0, 6]

      grille_a(:, 7) = [0, 6, 0,  0, 0, 0,  2, 8, 0]
      grille_a(:, 8) = [0, 0, 0,  4, 1, 9,  0, 0, 5]
      grille_a(:, 9) = [0, 0, 0,  0, 8, 0,  0, 7, 9]

      call ResoudreGrille(grille_a)

      ! la grille complétée (juste à côté de l'autre)
      grille_b(:, 1) = [5, 3, 4,  6, 7, 8,  9, 1, 2]
      grille_b(:, 2) = [6, 7, 2,  1, 9, 5,  3, 4, 8]
      grille_b(:, 3) = [1, 9, 8,  3, 4, 2,  5, 6, 7]

      grille_b(:, 4) = [8, 5, 9,  7, 6, 1,  4, 2, 3]
      grille_b(:, 5) = [4, 2, 6,  8, 5, 3,  7, 9, 1]
      grille_b(:, 6) = [7, 1, 3,  9, 2, 4,  8, 5, 6]

      grille_b(:, 7) = [9, 6, 1,  5, 3, 7,  2, 8, 4]
      grille_b(:, 8) = [2, 8, 7,  4, 1, 9,  6, 3, 5]
      grille_b(:, 9) = [3, 4, 5,  2, 8, 6,  1, 7, 9]

      ! pour comparer les deux
      outer: do i = 1, 9
         do j = 1, 9
            if (grille_a(i, j) /= grille_b(i, j)) then
               array_equality = .false.
               print *, "À i : ", i, "j : ", j, "grille_a : ", grille_a(i,j), &
                  "n'est pas égale à grille_b :", grille_b(i,j)
               exit outer
            end if
         end do
      end do outer

      if (array_equality .eqv. .false.) then
         print *, "The Wikipedia array assertion failed."
      else
         print *, "The Wikipedia array assertion was successful."
      end if
   end subroutine assert_wikipedia_solution
end program check

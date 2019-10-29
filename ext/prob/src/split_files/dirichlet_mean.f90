subroutine dirichlet_mean ( n, a, mean )

!*****************************************************************************80
!
!! DIRICHLET_MEAN returns the means of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be positive.
!
!    Output, real ( kind = 8 ) MEAN(N), the means of the PDF.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) mean(n)

  mean(1:n) = a(1:n)

  call r8vec_unit_sum ( n, mean )

  return
end

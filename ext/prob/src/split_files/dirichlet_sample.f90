subroutine dirichlet_sample ( n, a, seed, x )

!*****************************************************************************80
!
!! DIRICHLET_SAMPLE samples the Dirichlet PDF.
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
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 169.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be
!    positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a sample of the PDF.  The entries
!    of X should sum to 1.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a2
  real ( kind = 8 ) b2
  real ( kind = 8 ) c2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  a2 = 0.0D+00
  b2 = 1.0D+00

  do i = 1, n
    c2 = a(i)
    call gamma_sample ( a2, b2, c2, seed, x(i) )
  end do
!
!  Rescale the vector to have unit sum.
!
  call r8vec_unit_sum ( n, x )

  return
end

subroutine geometric_sample ( a, seed, x )

!*****************************************************************************80
!
!! GEOMETRIC_SAMPLE samples the Geometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x

  cdf = r8_uniform_01 ( seed )

  call geometric_cdf_inv ( cdf, a, x )

  return
end

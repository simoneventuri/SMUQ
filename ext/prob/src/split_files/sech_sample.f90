subroutine sech_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! SECH_SAMPLE samples the Hyperbolic Secant PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  cdf = r8_uniform_01 ( seed )

  x = a + b * log ( tan ( 0.5D+00 * r8_pi * cdf ) )

  return
end

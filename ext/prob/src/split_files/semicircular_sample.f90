subroutine semicircular_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! SEMICIRCULAR_SAMPLE samples the Semicircular PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
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
  real ( kind = 8 ) angle
  real ( kind = 8 ) b
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) radius
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  radius = r8_uniform_01 ( seed )
  radius = b * sqrt ( radius )
  angle = r8_pi * r8_uniform_01 ( seed )
  x = a + radius * cos ( angle )

  return
end

subroutine c8_normal_01_sample ( seed, x )

!*****************************************************************************80
!
!! C8_NORMAL_01_SAMPLE samples the complex Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, complex ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2
  complex ( kind = 8 ) x
  real ( kind = 8 ) x_c
  real ( kind = 8 ) x_r

  v1 = r8_uniform_01 ( seed )
  v2 = r8_uniform_01 ( seed )

  x_r = sqrt ( - 2.0D+00 * log ( v1 ) ) * cos ( 2.0D+00 * r8_pi * v2 )
  x_c = sqrt ( - 2.0D+00 * log ( v1 ) ) * sin ( 2.0D+00 * r8_pi * v2 )

  x = cmplx ( x_r, x_c, kind = 8 )

  return
end

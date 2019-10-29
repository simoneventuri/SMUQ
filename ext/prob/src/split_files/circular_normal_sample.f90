subroutine circular_normal_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_SAMPLE samples the Circular Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), a parameter of the PDF, the mean value.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF, the standard deviation.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(2), a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b
  real ( kind = 8 ) r
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2
  real ( kind = 8 ) x(2)

  v1 = r8_uniform_01 ( seed )
  v2 = r8_uniform_01 ( seed )

  r = sqrt ( - 2.0D+00 * log ( v1 ) )

  x(1) = a(1) + b * r * cos ( 2.0D+00 * r8_pi * v2 )
  x(2) = a(2) + b * r * sin ( 2.0D+00 * r8_pi * v2 )

  return
end

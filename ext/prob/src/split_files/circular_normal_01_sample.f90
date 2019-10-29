subroutine circular_normal_01_sample ( seed, x )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_SAMPLE samples the Circular Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
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
!    Output, real ( kind = 8 ) X(2), a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) v1
  real ( kind = 8 ) v2
  real ( kind = 8 ) x(2)

  v1 = r8_uniform_01 ( seed )
  v2 = r8_uniform_01 ( seed )

  x(1) = sqrt ( - 2.0D+00 * log ( v1 ) ) * cos ( 2.0D+00 * r8_pi * v2 )
  x(2) = sqrt ( - 2.0D+00 * log ( v1 ) ) * sin ( 2.0D+00 * r8_pi * v2 )

  return
end

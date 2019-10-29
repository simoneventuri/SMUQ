subroutine disk_sample ( a, b, c, seed, x1, x2 )

!*****************************************************************************80
!
!! DISK_SAMPLE samples points from a disk.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the disk.
!    The disk is centered at (A,B) and has radius C.
!    0.0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X1, X2, a sampled point of the disk.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) angle
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) radius_frac
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2

  radius_frac = r8_uniform_01 ( seed )
  radius_frac = sqrt ( radius_frac )

  angle = 2.0D+00 * r8_pi * r8_uniform_01 ( seed )

  x1 = a + c * radius_frac * cos ( angle )
  x2 = b + c * radius_frac * sin ( angle )

  return
end

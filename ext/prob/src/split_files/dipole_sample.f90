subroutine dipole_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! DIPOLE_SAMPLE samples the Dipole PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Knop,
!    Algorithm 441,
!    Random Deviates from the Dipole Distribution,
!    ACM Transactions on Mathematical Software,
!    Volume 16, Number 1, 1973, page 51.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A is arbitrary, but represents an angle, so only 0 <= A <= 2 * PI
!      is interesting,
!    and -1.0D+00 <= B <= 1.0D+00.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) b2
  real ( kind = 8 ) c2
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
!
!  Find (X1,X2) at random in a circle.
!
  a2 = b * sin ( a )
  b2 = b * cos ( a )
  c2 = 1.0D+00

  call disk_sample ( a2, b2, c2, seed, x1, x2 )
!
!  The dipole variate is the ratio X1 / X2.
!
  x = x1 / x2

  return
end

subroutine dipole_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! DIPOLE_CDF evaluates the Dipole CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A is arbitrary, but represents an angle, so only 0 <= A <= 2 * PI
!    is interesting, and -1.0D+00 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  cdf = 0.5D+00 + ( 1.0D+00 / r8_pi ) * atan ( x ) &
    + b * b * ( x * cos ( 2.0D+00 * a ) &
    - sin ( 2.0D+00 * a ) ) / ( r8_pi * ( 1.0D+00 + x * x ) )

  return
end

subroutine cardioid_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! CARDIOID_CDF evaluates the Cardioid CDF.
!
!  Discussion:
!
!    The angle X is assumed to lie between A - PI and A + PI.
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
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    A - PI <= X <= A + PI.
!
!    Input, real ( kind = 8 ) A, B, the parameters.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( x <= a - r8_pi ) then
    cdf = 0.0D+00
  else if ( x < a + r8_pi ) then
    cdf = ( r8_pi + x - a + 2.0D+00 * b * sin ( x - a ) ) / ( 2.0D+00 * r8_pi )
  else
    cdf = 1.0D+00
  end if

  return
end

subroutine chebyshev1_cdf ( x, cdf )

!*****************************************************************************80
!
!! CHEBYSHEV1_CDF evaluates the Chebyshev1 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( x < -1.0D+00 ) then
    cdf = 0.0D+00
  else if ( 1.0D+00 < x ) then
    cdf = 1.0D+00
  else
    cdf = 0.5 + asin ( x ) / r8_pi
  end if

  return
end

subroutine anglit_cdf ( x, cdf )

!*****************************************************************************80
!
!! ANGLIT_CDF evaluates the Anglit CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 December 1999
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

  if ( x <  - 0.25D+00 * r8_pi ) then
    cdf = 0.0D+00
  else if ( x < 0.25D+00 * r8_pi ) then
    cdf = 0.5D+00 - 0.5D+00 * cos ( 2.0D+00 * x + r8_pi / 2.0D+00 )
  else
    cdf = 1.0D+00
  end if

  return
end

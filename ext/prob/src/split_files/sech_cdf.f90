subroutine sech_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! SECH_CDF evaluates the Hyperbolic Secant CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = ( x - a ) / b

  cdf = 2.0D+00 * atan ( exp ( y ) ) / r8_pi

  return
end

subroutine semicircular_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! SEMICIRCULAR_CDF evaluates the Semicircular CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
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

  if ( x <= a - b ) then

    cdf = 0.0D+00

  else if ( x <= a + b ) then

    y = ( x - a ) / b

    cdf = 0.5D+00 + ( y * sqrt ( 1.0D+00 - y ** 2 ) + asin ( y ) ) / r8_pi

  else if ( a + b < x ) then

    cdf = 1.0D+00

  end if

  return
end

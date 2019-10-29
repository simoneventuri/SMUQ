subroutine cosine_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! COSINE_CDF evaluates the Cosine CDF.
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

  if ( x <= a - r8_pi * b ) then

    cdf = 0.0D+00

  else if ( x <= a + r8_pi * b ) then

    y = ( x - a ) / b

    cdf = 0.5D+00 + ( y + sin ( y ) ) / ( 2.0D+00 * r8_pi )

  else if ( a + r8_pi * b < x ) then

    cdf = 1.0D+00

  end if

  return
end

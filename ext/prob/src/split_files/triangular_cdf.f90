subroutine triangular_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! TRIANGULAR_CDF evaluates the Triangular CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
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
!    A < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( x <= a ) then
    cdf = 0.0D+00
  else if ( x <= 0.5D+00 * ( a + b ) ) then
    cdf = 2.0D+00 * ( x ** 2 - 2.0D+00 * a * x + a ** 2 ) / ( b - a ) ** 2
  else if ( x <= b ) then
    cdf = 0.5D+00 + ( - 2.0D+00 * x ** 2 + 4.0D+00 * b * x + 0.5D+00 * a ** 2 &
      - a * b - 1.5D+00 * b ** 2 ) / ( b - a ) ** 2
  else
    cdf = 1.0D+00
  end if

  return
end

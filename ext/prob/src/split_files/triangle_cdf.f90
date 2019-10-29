subroutine triangle_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! TRIANGLE_CDF evaluates the Triangle CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( x <= a ) then

    cdf = 0.0D+00

  else if ( x <= b ) then

    if ( a == b ) then
      cdf = 0.0D+00
    else
      cdf = ( x - a ) * ( x - a ) / ( b - a ) / ( c - a )
    end if

  else if ( x <= c ) then

    cdf = ( b - a ) / ( c - a ) &
        + ( 2.0D+00 * c - b - x ) * ( x - b ) / ( c - b ) / ( c - a )

  else

    cdf = 1.0D+00

  end if

  return
end

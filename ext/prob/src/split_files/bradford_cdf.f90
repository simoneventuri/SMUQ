subroutine bradford_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! BRADFORD_CDF evaluates the Bradford CDF.
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
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
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
    cdf = log ( 1.0D+00 + c * ( x - a ) / ( b - a ) ) / log ( c + 1.0D+00 )
  else if ( b < x ) then
    cdf = 1.0D+00
  end if

  return
end

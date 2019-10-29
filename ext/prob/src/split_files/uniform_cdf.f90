subroutine uniform_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! UNIFORM_CDF evaluates the Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
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

  if ( x < a ) then
    cdf = 0.0D+00
  else if ( b < x ) then
    cdf = 1.0D+00
  else
    cdf = ( x - a ) / ( b - a )
  end if

  return
end

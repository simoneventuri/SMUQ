subroutine log_uniform_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! LOG_UNIFORM_CDF evaluates the Log Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
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
!    0.0 < B.
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
  else if ( x < b ) then
    cdf = ( log ( x ) - log ( a ) ) / ( log ( b ) - log ( a ) )
  else
    cdf = 1.0D+00
  end if

  return
end

subroutine uniform_discrete_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! UNIFORM_DISCRETE_CDF evaluates the Uniform Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x

  if ( x < a ) then
    cdf = 0.0D+00
  else if ( b < x ) then
    cdf = 1.0D+00
  else
    cdf = real ( x + 1 - a, kind = 8 ) / real ( b + 1 - a, kind = 8 )
  end if

  return
end

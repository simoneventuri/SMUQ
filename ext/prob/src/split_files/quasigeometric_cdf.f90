subroutine quasigeometric_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! QUASIGEOMETRIC_CDF evaluates the Quasigeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the maximum number of trials.
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x

  if ( x < 0 ) then
    cdf = 0.0D+00
  else if ( x == 0 ) then
    cdf = a
  else if ( b == 0.0D+00 ) then
    cdf = 1.0D+00
  else
    cdf = a + ( 1.0D+00 - a ) * ( 1.0D+00 - b**x )
  end if

  return
end

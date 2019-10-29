subroutine discrete_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! DISCRETE_CDF evaluates the Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the item whose probability is desired.
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of outcomes
!    1 through A.  Each entry must be nonnegative.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x

  if ( x < 1 ) then
    cdf = 0.0D+00
  else if ( x < a ) then
    cdf = sum ( b(1:x) ) / sum ( b(1:a) )
  else if ( a <= x ) then
    cdf = 1.0D+00
  end if

  return
end

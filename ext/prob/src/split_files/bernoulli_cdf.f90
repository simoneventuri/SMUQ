subroutine bernoulli_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! BERNOULLI_CDF evaluates the Bernoulli CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of successes on a single trial.
!    X = 0 or 1.
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x

  if ( x < 0 ) then
    cdf = 0.0D+00
  else if ( x == 0 ) then
    cdf = 1.0D+00 - a
  else
    cdf = 1.0D+00
  end if

  return
end

subroutine geometric_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! GEOMETRIC_CDF evaluates the Geometric CDF.
!
!  Discussion:
!
!    CDF(X,P) is the probability that there will be at least one
!    successful trial in the first X Bernoulli trials, given that
!    the probability of success in a single trial is P.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the maximum number of trials.
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

  if ( x <= 0 ) then
    cdf = 0.0D+00
  else if ( a == 0.0D+00 ) then
    cdf = 0.0D+00
  else if ( a == 1.0D+00 ) then
    cdf = 1.0D+00
  else
    cdf = 1.0D+00 - ( 1.0D+00 - a ) ** x
  end if

  return
end

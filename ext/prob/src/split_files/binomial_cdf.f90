subroutine binomial_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! BINOMIAL_CDF evaluates the Binomial CDF.
!
!  Discussion:
!
!    CDF(X)(A,B) is the probability of at most X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!    A sequence of trials with fixed probability of success on
!    any trial is known as a sequence of Bernoulli trials.
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
!    Input, integer ( kind = 4 ) X, the desired number of successes.
!    0 <= X <= A.
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) cnk
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) j
  real ( kind = 8 ) pr
  integer ( kind = 4 ) x

  if ( x < 0 ) then

    cdf = 0.0D+00

  else if ( a <= x ) then

    cdf = 1.0D+00

  else if ( b == 0.0D+00 ) then

    cdf = 1.0D+00

  else if ( b == 1.0D+00 ) then

    cdf = 0.0D+00

  else

    cdf = 0.0D+00

    do j = 0, x

      cnk = i4_choose ( a, j )

      pr = real ( cnk, kind = 8 ) * b ** j * ( 1.0D+00 - b ) ** ( a - j )

      cdf = cdf + pr

    end do

  end if

  return
end

subroutine cumnbn ( f, s, pr, ompr, cum, ccum )

!*****************************************************************************80
!
!! CUMNBN evaluates the cumulative negative binomial distribution.
!
!  Discussion:
!
!    This routine returns the probability that there will be F or
!    fewer failures before there are S successes, with each binomial
!    trial having a probability of success PR.
!
!    Prob(# failures = F | S successes, PR)  =
!                        ( S + F - 1 )
!                        (            ) * PR^S * (1-PR)^F
!                        (      F     )
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.26.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the number of failures.
!
!    Input, real ( kind = 8 ) S, the number of successes.
!
!    Input, real ( kind = 8 ) PR, OMPR, the probability of success on
!    each binomial trial, and the value of (1-PR).
!
!    Output, real ( kind = 8 ) CUM, CCUM, the negative binomial CDF,
!    and the complementary CDF.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) f
  real ( kind = 8 ) ompr
  real ( kind = 8 ) pr
  real ( kind = 8 ) s

  call cumbet ( pr, ompr, s, f+1.D+00, cum, ccum )

  return
end

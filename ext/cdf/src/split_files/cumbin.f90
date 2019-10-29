subroutine cumbin ( s, xn, pr, ompr, cum, ccum )

!*****************************************************************************80
!
!! CUMBIN evaluates the cumulative binomial distribution.
!
!  Discussion:
!
!    This routine returns the probability of 0 to S successes in XN binomial
!    trials, each of which has a probability of success, PR.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.24.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the upper limit of summation.
!
!    Input, real ( kind = 8 ) XN, the number of trials.
!
!    Input, real ( kind = 8 ) PR, the probability of success in one trial.
!
!    Input, real ( kind = 8 ) OMPR, equals ( 1 - PR ).
!
!    Output, real ( kind = 8 ) CUM, the cumulative binomial distribution.
!
!    Output, real ( kind = 8 ) CCUM, the complement of the cumulative
!    binomial distribution.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) ompr
  real ( kind = 8 ) pr
  real ( kind = 8 ) s
  real ( kind = 8 ) xn

  if ( s < xn ) then

    call cumbet ( pr, ompr, s + 1.0D+00, xn - s, ccum, cum )

  else

    cum = 1.0D+00
    ccum = 0.0D+00

  end if

  return
end

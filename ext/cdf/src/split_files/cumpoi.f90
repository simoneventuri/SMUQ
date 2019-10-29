subroutine cumpoi ( s, xlam, cum, ccum )

!*****************************************************************************80
!
!! CUMPOI evaluates the cumulative Poisson distribution.
!
!  Discussion:
!
!    This routine returns the probability of S or fewer events in a Poisson
!    distribution with mean XLAM.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    Formula 26.4.21.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the upper limit of cumulation of the
!    Poisson density function.
!
!    Input, real ( kind = 8 ) XLAM, the mean of the Poisson distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the Poisson density CDF and
!    complementary CDF.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) chi
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) s
  real ( kind = 8 ) xlam

  df =  2.0D+00  * ( s + 1.0D+00 )
  chi =  2.0D+00  * xlam

  call cumchi ( chi, df, ccum, cum )

  return
end

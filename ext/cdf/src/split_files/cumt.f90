subroutine cumt ( t, df, cum, ccum )

!*****************************************************************************80
!
!! CUMT evaluates the cumulative T distribution.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    Formula 26.5.27.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom of
!    the T distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the T distribution CDF and
!    complementary CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) df
  real ( kind = 8 ) oma
  real ( kind = 8 ) t
  real ( kind = 8 ) xx
  real ( kind = 8 ) yy

  xx = df / ( df + t**2 )
  yy = t**2 / ( df + t**2 )

  call cumbet ( xx, yy, 0.5D+00*df, 0.5D+00, a, oma )

  if ( t <= 0.0D+00 ) then
    cum = 0.5D+00 * a
    ccum = oma + cum
  else
    ccum = 0.5D+00 * a
    cum = oma + ccum
  end if

  return
end

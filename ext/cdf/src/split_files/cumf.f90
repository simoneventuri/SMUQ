subroutine cumf ( f, dfn, dfd, cum, ccum )

!*****************************************************************************80
!
!! CUMF evaluates the cumulative F distribution.
!
!  Discussion:
!
!    This routine computes the integral from 0 to F of the F density with DFN
!    numerator and DFD denominator degrees of freedom.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.28.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DFN, DFD, the number of degrees of
!    freedom for the numerator and denominator.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the value of the F CDF and
!    the complementary F CDF.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dfd
  real ( kind = 8 ) dfn
  real ( kind = 8 ) dsum
  real ( kind = 8 ) f
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) prod
  real ( kind = 8 ) xx
  real ( kind = 8 ) yy

  if ( f <= 0.0D+00 ) then
    cum = 0.0D+00
    ccum = 1.0D+00
    return
  end if

  prod = dfn * f
!
!  XX is such that the incomplete beta with parameters
!  DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM
!
!  YY is 1 - XX
!
!  Calculate the smaller of XX and YY accurately.
!
  dsum = dfd + prod
  xx = dfd / dsum

  if ( 0.5D+00 < xx ) then
    yy = prod / dsum
    xx = 1.0D+00 - yy
  else
    yy = 1.0D+00 - xx
  end if

  call beta_inc ( 0.5D+00 * dfd, 0.5D+00 * dfn, xx, yy, ccum, cum, ierr )

  return
end

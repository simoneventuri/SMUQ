subroutine cumbet ( x, y, a, b, cum, ccum )

!*****************************************************************************80
!
!! CUMBET evaluates the cumulative incomplete beta distribution.
!
!  Discussion:
!
!    This routine calculates the CDF to X of the incomplete beta distribution
!    with parameters A and B.  This is the integral from 0 to x
!    of (1/B(a,b))*f(t)) where f(t) = t**(a-1) * (1-t)**(b-1)
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) Y, the value of 1-X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the values of the cumulative
!    density function and complementary cumulative density function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= 0.0D+00 ) then

    cum = 0.0
    ccum = 1.0D+00

  else if ( y <= 0.0D+00 ) then

    cum = 1.0D+00
    ccum = 0.0

  else

    call beta_inc ( a, b, x, y, cum, ccum, ierr )

  end if

  return
end

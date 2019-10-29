function rcomp ( a, x )

!*****************************************************************************80
!
!! RCOMP evaluates exp(-X) * X**A / Gamma(A).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, X, arguments of the quantity to be computed.
!
!    Output, real ( kind = 8 ) RCOMP, the value of exp(-X) * X**A / Gamma(A).
!
!  Local parameters:
!
!    RT2PIN = 1/SQRT(2*PI)
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) gam1
  real ( kind = 8 ) gamma
  real ( kind = 8 ) rcomp
  real ( kind = 8 ) rlog
  real ( kind = 8 ), parameter :: rt2pin = 0.398942280401433D+00
  real ( kind = 8 ) t
  real ( kind = 8 ) t1
  real ( kind = 8 ) u
  real ( kind = 8 ) x

  if ( a < 20.0D+00 ) then

    t = a * log ( x ) - x

    if ( a < 1.0D+00 ) then
      rcomp = ( a * exp ( t ) ) * ( 1.0D+00 + gam1 ( a ) )
    else
      rcomp = exp ( t ) / gamma ( a )
    end if

  else

    u = x / a

    if ( u == 0.0D+00 ) then
      rcomp = 0.0D+00
    else
      t = ( 1.0D+00 / a )**2
      t1 = ((( 0.75D+00 * t - 1.0D+00 ) * t + 3.5D+00 ) * t - 105.0D+00 ) &
        / ( a * 1260.0D+00 )
      t1 = t1 - a * rlog ( u )
      rcomp = rt2pin * sqrt ( a ) * exp ( t1 )
    end if

  end if

  return
end

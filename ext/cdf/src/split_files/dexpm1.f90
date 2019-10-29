function dexpm1 ( x )

!*****************************************************************************80
!
!! DEXPM1 evaluates the function EXP(X) - 1.
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
!    Input, real ( kind = 8 ) X, the value at which exp(X)-1 is desired.
!
!    Output, real ( kind = 8 ) DEXPM1, the value of exp(X)-1.
!
  implicit none

  real ( kind = 8 ) bot
  real ( kind = 8 ) dexpm1
  real ( kind = 8 ), parameter :: p1 =  0.914041914819518D-09
  real ( kind = 8 ), parameter :: p2 =  0.238082361044469D-01
  real ( kind = 8 ), parameter :: q1 = -0.499999999085958D+00
  real ( kind = 8 ), parameter :: q2 =  0.107141568980644D+00
  real ( kind = 8 ), parameter :: q3 = -0.119041179760821D-01
  real ( kind = 8 ), parameter :: q4 =  0.595130811860248D-03
  real ( kind = 8 ) top
  real ( kind = 8 ) w
  real ( kind = 8 ) x

  if ( abs ( x ) <= 0.15D+00 ) then

    top = ( p2 * x + p1 ) * x + 1.0D+00
    bot = ((( q4 * x + q3 ) * x + q2 ) * x + q1 ) * x + 1.0D+00
    dexpm1 = x * ( top / bot )

  else

    w = exp ( x )

    if ( x <= 0.0D+00 ) then
      dexpm1 = ( w - 0.5D+00 ) - 0.5D+00
    else
      dexpm1 = w * ( 0.5D+00 &
        + ( 0.5D+00 - 1.0D+00 / w ))
    end if

  end if

  return
end

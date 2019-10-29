function rlog1 ( x )

!*****************************************************************************80
!
!! RLOG1 evaluates the function X - ln ( 1 + X ).
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
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) RLOG1, the value of X - ln ( 1 + X ).
!
  implicit none

  real ( kind = 8 ), parameter :: a = 0.566749439387324D-01
  real ( kind = 8 ), parameter :: b = 0.456512608815524D-01
  real ( kind = 8 ) h
  real ( kind = 8 ), parameter :: half = 0.5D+00
  real ( kind = 8 ), parameter :: p0 = 0.333333333333333D+00
  real ( kind = 8 ), parameter :: p1 = -0.224696413112536D+00
  real ( kind = 8 ), parameter :: p2 = 0.620886815375787D-02
  real ( kind = 8 ), parameter :: q1 = -0.127408923933623D+01
  real ( kind = 8 ), parameter :: q2 = 0.354508718369557D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) rlog1
  real ( kind = 8 ) t
  real ( kind = 8 ), parameter :: two =  2.0D+00
  real ( kind = 8 ) w
  real ( kind = 8 ) w1
  real ( kind = 8 ) x

  if ( x < -0.39D+00 ) then

    w = ( x + half ) + half
    rlog1 = x - log ( w )

  else if ( x < -0.18D+00 ) then

    h = x + 0.3D+00
    h = h / 0.7D+00
    w1 = a - h * 0.3D+00

    r = h / ( h + 2.0D+00 )
    t = r * r
    w = ( ( p2 * t + p1 ) * t + p0 ) / ( ( q2 * t + q1 ) * t + 1.0D+00 )
    rlog1 = two * t * ( 1.0D+00 / ( 1.0D+00 - r ) - r * w ) + w1

  else if ( x <= 0.18D+00 ) then

    h = x
    w1 = 0.0D+00

    r = h / ( h + two )
    t = r * r
    w = ( ( p2 * t + p1 ) * t + p0 ) / ( ( q2 * t + q1 ) * t + 1.0D+00 )
    rlog1 = two * t * ( 1.0D+00 / ( 1.0D+00 - r ) - r * w ) + w1

  else if ( x <= 0.57D+00 ) then

    h = 0.75D+00 * x - 0.25D+00
    w1 = b + h / 3.0D+00

    r = h / ( h + 2.0D+00 )
    t = r * r
    w = ( ( p2 * t + p1 ) * t + p0 ) / ( ( q2 * t + q1 ) * t + 1.0D+00 )
    rlog1 = two * t * ( 1.0D+00 / ( 1.0D+00 - r ) - r * w ) + w1

  else

    w = ( x + half ) + half
    rlog1 = x - log ( w )

  end if

  return
end

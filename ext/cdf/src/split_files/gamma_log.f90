function gamma_log ( a )

!*****************************************************************************80
!
!! GAMMA_LOG evaluates ln ( Gamma ( A ) ) for positive A.
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
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
!    Input, real ( kind = 8 ) A, the argument of the function.
!    A should be positive.
!
!    Output, real ( kind = 8 ), GAMMA_LOG, the value of ln ( Gamma ( A ) ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: c0 =  0.833333333333333D-01
  real ( kind = 8 ), parameter :: c1 = -0.277777777760991D-02
  real ( kind = 8 ), parameter :: c2 =  0.793650666825390D-03
  real ( kind = 8 ), parameter :: c3 = -0.595202931351870D-03
  real ( kind = 8 ), parameter :: c4 =  0.837308034031215D-03
  real ( kind = 8 ), parameter :: c5 = -0.165322962780713D-02
  real ( kind = 8 ), parameter :: d  =  0.418938533204673D+00
  real ( kind = 8 ) gamma_log
  real ( kind = 8 ) gamma_ln1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) t
  real ( kind = 8 ) w

  if ( a <= 0.8D+00 ) then

    gamma_log = gamma_ln1 ( a ) - log ( a )

  else if ( a <= 2.25D+00 ) then

    t = ( a - 0.5D+00 ) - 0.5D+00
    gamma_log = gamma_ln1 ( t )

  else if ( a < 10.0D+00 ) then

    n = a - 1.25D+00
    t = a
    w = 1.0D+00
    do i = 1, n
      t = t - 1.0D+00
      w = t * w
    end do

    gamma_log = gamma_ln1 ( t - 1.0D+00 ) + log ( w )

  else

    t = ( 1.0D+00 / a )**2

    w = ((((( c5 * t + c4 ) * t + c3 ) * t + c2 ) * t + c1 ) * t + c0 ) / a

    gamma_log = ( d + w ) + ( a - 0.5D+00 ) &
      * ( log ( a ) - 1.0D+00 )

  end if

  return
end

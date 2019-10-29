function esum ( mu, x )

!*****************************************************************************80
!
!! ESUM evaluates exp ( MU + X ).
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
!    Input, integer ( kind = 4 ) MU, part of the argument.
!
!    Input, real ( kind = 8 ) X, part of the argument.
!
!    Output, real ( kind = 8 ) ESUM, the value of exp ( MU + X ).
!
  implicit none

  real ( kind = 8 ) esum
  integer ( kind = 4 ) mu
  real ( kind = 8 ) w
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then
    if ( 0 <= mu ) then
      w = mu + x
      if ( w <= 0.0D+00 ) then
        esum = exp ( w )
        return
      end if
    end if
  else if ( 0.0D+00 < x ) then
    if ( mu <= 0 ) then
      w = mu + x
      if ( 0.0D+00 <= w ) then
        esum = exp ( w )
        return
      end if
    end if
  end if

  w = mu
  esum = exp ( w ) * exp ( x )

  return
end

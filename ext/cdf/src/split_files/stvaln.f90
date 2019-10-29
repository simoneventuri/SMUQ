function stvaln ( p )

!*****************************************************************************80
!
!! STVALN provides starting values for the inverse of the normal distribution.
!
!  Discussion:
!
!    The routine returns an X for which it is approximately true that
!      P = CUMNOR(X),
!    that is,
!      P = Integral ( -infinity < U <= X ) exp(-U*U/2)/sqrt(2*PI) dU.
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980, page 95,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the probability whose normal deviate
!    is sought.
!
!    Output, real ( kind = 8 ) STVALN, the normal deviate whose probability
!    is approximately P.
!
  implicit none

  real ( kind = 8 ) eval_pol
  real ( kind = 8 ) p
  real ( kind = 8 ) sgn
  real ( kind = 8 ) stvaln
  real ( kind = 8 ), parameter, dimension(0:4) :: xden = (/ &
    0.993484626060D-01, &
    0.588581570495D+00, &
    0.531103462366D+00, &
    0.103537752850D+00, &
    0.38560700634D-02 /)
  real ( kind = 8 ), parameter, dimension(0:4) :: xnum = (/ &
    -0.322232431088D+00, &
    -1.000000000000D+00, &
    -0.342242088547D+00, &
    -0.204231210245D-01, &
    -0.453642210148D-04 /)
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  if ( p <= 0.5D+00 ) then

    sgn = -1.0D+00
    z = p

  else

    sgn = 1.0D+00
    z = 1.0D+00 - p

  end if

  y = sqrt ( -2.0D+00 * log ( z ) )
  stvaln = y + eval_pol ( xnum, 4, y ) / eval_pol ( xden, 4, y )
  stvaln = sgn * stvaln

  return
end

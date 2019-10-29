function dinvnr ( p, q )

!*****************************************************************************80
!
!! DINVNR computes the inverse of the normal distribution.
!
!  Discussion:
!
!    This routine returns X such that
!
!      CUMNOR(X) = P,
!
!    that is, so that
!
!      P = integral ( -oo <= T <= X ) exp(-U*U/2)/sqrt(2*PI) dU
!
!    The rational function on page 95 of Kennedy and Gentle is used as a
!    starting value for the Newton method of finding roots.
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, Q, the probability, and the complementary
!    probability.
!
!    Output, real ( kind = 8 ) DINVNR, the argument X for which the
!    Normal CDF has the value P.
!
  implicit none

  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dinvnr
  real ( kind = 8 ) dx
  real ( kind = 8 ), parameter :: eps = 1.0D-13
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: maxit = 100
  real ( kind = 8 ) p
  real ( kind = 8 ) pp
  real ( kind = 8 ) q
  real ( kind = 8 ), parameter :: r2pi = 0.3989422804014326D+00
  real ( kind = 8 ) strtx
  real ( kind = 8 ) stvaln
  real ( kind = 8 ) xcur

  pp = min ( p, q )
  strtx = stvaln ( pp )
  xcur = strtx
!
!  Newton iterations.
!
  do i = 1, maxit

    call cumnor ( xcur, cum, ccum )
    dx = ( cum - pp ) / ( r2pi * exp ( -0.5D+00 * xcur * xcur ) )
    xcur = xcur - dx

    if ( abs ( dx / xcur ) < eps ) then
      if ( p <= q ) then
        dinvnr = xcur
      else
        dinvnr = -xcur
      end if
      return
    end if

  end do

  if ( p <= q ) then
    dinvnr = strtx
  else
    dinvnr = -strtx
  end if

  return
end

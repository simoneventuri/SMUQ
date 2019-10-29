function fpser ( a, b, x, eps )

!*****************************************************************************80
!
!! FPSER evaluates IX(A,B)(X) for very small B.
!
!  Discussion:
!
!    This routine is appropriate for use when
!
!      B < min ( EPS, EPS * A )
!
!    and
!
!      X <= 0.5.
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
!    Input, real ( kind = 8 ) A, B, parameters of the function.
!
!    Input, real ( kind = 8 ) X, the point at which the function is to
!    be evaluated.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) FPSER, the value of IX(A,B)(X).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) an
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) eps
  real ( kind = 8 ) exparg
  real ( kind = 8 ) fpser
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) x

  fpser = 1.0D+00

  if ( 1.0D-03 * eps < a ) then
    fpser = 0.0D+00
    t = a * log ( x )
    if ( t < exparg ( 1 ) ) then
      return
    end if
    fpser = exp ( t )
  end if
!
!  1/B(A,B) = B
!
  fpser = ( b / a ) * fpser
  tol = eps / a
  an = a + 1.0D+00
  t = x
  s = t / an

  do

    an = an + 1.0D+00
    t = x * t
    c = t / an
    s = s + c

    if ( abs ( c ) <= tol ) then
      exit
    end if

  end do

  fpser = fpser * ( 1.0D+00 + a * s )

  return
end

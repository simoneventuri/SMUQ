function apser ( a, b, x, eps )

!*****************************************************************************80
!
!! APSER computes the incomplete beta ratio I(SUB(1-X))(B,A).
!
!  Discussion:
!
!    APSER is used only for cases where
!
!      A <= min ( EPS, EPS * B ),
!      B * X <= 1, and
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
!    Input, real ( kind = 8 ) A, B, X, the parameters of the
!    incomplete beta ratio.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) APSER, the computed value of the
!    incomplete beta ratio.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) aj
  real ( kind = 8 ) apser
  real ( kind = 8 ) b
  real ( kind = 8 ) bx
  real ( kind = 8 ) c
  real ( kind = 8 ) eps
  real ( kind = 8 ), parameter :: g = 0.577215664901533D+00
  real ( kind = 8 ) j
  real ( kind = 8 ) psi
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) x

  bx = b * x
  t = x - bx

  if ( b * eps <= 0.02D+00 ) then
    c = log ( x ) + psi ( b ) + g + t
  else
    c = log ( bx ) + g + t
  end if

  tol = 5.0D+00 * eps * abs ( c )
  j = 1.0D+00
  s = 0.0D+00

  do

    j = j + 1.0D+00
    t = t * ( x - bx / j )
    aj = t / j
    s = s + aj

    if ( abs ( aj ) <= tol ) then
      exit
    end if

  end do

  apser = -a * ( c + s )

  return
end

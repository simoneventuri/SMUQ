function trigamma ( x )

!*****************************************************************************80
!
!! TRIGAMMA calculates the TriGamma function.
!
!  Discussion:
!
!    TriGamma(x) = d^2 log ( Gamma ( x ) ) / dx^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    FORTRAN77 original version by B Schneider
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    BE Schneider,
!    Algorithm AS 121:
!    Trigamma Function,
!    Applied Statistics,
!    Volume 27, Number 1, page 97-99, 1978.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the trigamma function.
!    0 < X.
!
!    Output, real ( kind = 8 ) TRIGAMMA, the value of the
!    trigamma function at X.
!
  implicit none

  real ( kind = 8 ), parameter :: a = 0.0001D+00
  real ( kind = 8 ), parameter :: b = 5.0D+00
  real ( kind = 8 ), parameter :: b2 =   1.0D+00 / 6.0D+00
  real ( kind = 8 ), parameter :: b4 = - 1.0D+00 / 30.0D+00
  real ( kind = 8 ), parameter :: b6 =   1.0D+00 / 42.0D+00
  real ( kind = 8 ), parameter :: b8 = - 1.0D+00 / 30.0D+00
  real ( kind = 8 ) trigamma
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z
!
!  1): If X is not positive, fail.
!
  if ( x <= 0.0D+00 ) then

    trigamma = 0.0D+00
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIGAMMA - Fatal error!'
    write ( *, '(a)' ) '  X <= 0.'
    stop 1
!
!  2): If X is smaller than A, use a small value approximation.
!
  else if ( x <= a ) then

    trigamma = 1.0D+00 / x ** 2
!
!  3): Otherwise, increase the argument to B <= ( X + I ).
!
  else

    z = x
    trigamma = 0.0D+00

    do while ( z < b )
      trigamma = trigamma + 1.0D+00 / z ** 2
      z = z + 1.0D+00
    end do
!
!  ...and then apply an asymptotic formula.
!
    y = 1.0D+00 / z ** 2

    trigamma = trigamma + 0.5D+00 * &
            y + ( 1.0D+00 &
          + y * ( b2 &
          + y * ( b4 &
          + y * ( b6 &
          + y *   b8 )))) / z

  end if

  return
end

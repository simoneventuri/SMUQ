function digamma ( x )

!*****************************************************************************80
!
!! DIGAMMA calculates the digamma or Psi function.
!
!  Discussion:
!
!    DiGamma ( X ) = d ( log ( Gamma ( X ) ) ) / dX
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
!    Original FORTRAN77 version by Jose Bernardo.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jose Bernardo,
!    Algorithm AS 103:
!    Psi ( Digamma ) Function,
!    Applied Statistics,
!    Volume 25, Number 3, pages 315-317, 1976.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the digamma function.
!    0 < X.
!
!    Output, real ( kind = 8 ) DIGAMMA, the value of the digamma function at X.
!
  implicit none

  real ( kind = 8 ), parameter :: c = 8.5D+00
  real ( kind = 8 ), parameter :: euler_mascheroni = 0.57721566490153286060D+00
  real ( kind = 8 ) digamma
  real ( kind = 8 ) r
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
!
!  Check the input.
!
  if ( x <= 0.0D+00 ) then
    digamma = 0.0D+00
    return
  end if
!
!  Approximation for small argument.
!
  if ( x <= 0.000001D+00 ) then
    digamma = - euler_mascheroni - 1.0D+00 / x + 1.6449340668482264365D+00 * x
    return
  end if
!
!  Reduce to DIGAMA(X + N).
!
  digamma = 0.0D+00
  x2 = x

  do while ( x2 < c )
    digamma = digamma - 1.0D+00 / x2
    x2 = x2 + 1.0D+00
  end do
!
!  Use Stirling's (actually de Moivre's) expansion.
!
  r = 1.0D+00 / x2

  digamma = digamma + log ( x2 ) - 0.5D+00 * r

  r = r * r

  digamma = digamma &
    - r * ( 1.0D+00 / 12.0D+00 &
    - r * ( 1.0D+00 / 120.0D+00 &
    - r * ( 1.0D+00 / 252.0D+00 &
    - r * ( 1.0D+00 / 240.0D+00 &
    - r * ( 1.0D+00 / 132.0D+00 ) ) ) ) )

  return
end

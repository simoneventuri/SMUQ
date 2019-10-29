subroutine dipole_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! DIPOLE_PDF evaluates the Dipole PDF.
!
!  Discussion:
!
!    PDF(A,B;X) =
!        1 / ( PI * ( 1 + X^2 ) )
!      + B^2 * ( ( 1 - X^2 ) * cos ( 2 * A ) + 2 * X * sin ( 2 * A ) )
!      / ( PI * ( 1 + X^2 )^2 )
!
!    Densities of this kind commonly occur in the analysis of resonant
!    scattering of elementary particles.
!
!    DIPOLE_PDF(A,0;X) = CAUCHY_PDF(A;X)
!
!    A = 0, B = 1 yields the single channel dipole distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Knop,
!    Algorithm 441,
!    Random Deviates from the Dipole Distribution,
!    ACM Transactions on Mathematical Software,
!    Volume 16, Number 1, 1973, page 51.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A is arbitrary, but represents an angle, so only 0 <= A <= 2 * PI
!      is interesting,
!    and -1.0 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  pdf = 1.0D+00 / ( r8_pi * ( 1.0D+00 + x * x ) ) &
    + b * b * ( ( 1.0D+00 - x * x ) * cos ( 2.0D+00 * a ) &
    + 2.0D+00 * x * sin ( 2.0D+00 * x ) ) / ( r8_pi * ( 1.0D+00 + x * x ) ** 2 )

  return
end

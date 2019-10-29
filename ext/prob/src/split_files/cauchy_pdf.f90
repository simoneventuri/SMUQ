subroutine cauchy_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! CAUCHY_PDF evaluates the Cauchy PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 1 / ( PI * B * ( 1 + ( ( X - A ) / B )^2 ) )
!
!    The Cauchy PDF is also known as the Breit-Wigner PDF.  It
!    has some unusual properties.  In particular, the integrals for the
!    expected value and higher order moments are "singular", in the
!    sense that the limiting values do not exist.  A result can be
!    obtained if the upper and lower limits of integration are set
!    equal to +T and -T, and the limit as T => +oo is taken, but
!    this is a very weak and unreliable sort of limit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = ( x - a ) / b

  pdf = 1.0D+00 / ( r8_pi * b * ( 1.0D+00 + y * y ) )

  return
end

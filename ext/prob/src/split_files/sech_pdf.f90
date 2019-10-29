subroutine sech_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! SECH_PDF evaluates the Hypebolic Secant PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = sech ( ( X - A ) / B ) / ( PI * B )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
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
  real ( kind = 8 ) sech
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = ( x - a ) / b

  pdf = sech ( y ) / ( r8_pi * b )

  return
end

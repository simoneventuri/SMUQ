subroutine normal_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! NORMAL_PDF evaluates the Normal PDF.
!
!  Discussion:
!
!    PDF(A,B;X)
!      = exp ( - 0.5D+00 * ( ( X - A ) / B )^2 ) / ( B * sqrt ( 2 * PI ) )
!
!    The normal PDF is also known as the Gaussian PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
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

  pdf = exp ( - 0.5D+00 * y * y )  / ( b * sqrt ( 2.0D+00 * r8_pi ) )

  return
end

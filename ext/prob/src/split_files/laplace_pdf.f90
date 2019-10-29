subroutine laplace_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! LAPLACE_PDF evaluates the Laplace PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = exp ( - abs ( X - A ) / B ) / ( 2 * B )
!
!    The Laplace PDF is also known as the Double Exponential PDF.
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
  real ( kind = 8 ) x

  pdf = exp ( - abs ( x - a ) / b ) / ( 2.0D+00 * b )

  return
end

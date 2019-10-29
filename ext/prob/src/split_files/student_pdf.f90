subroutine student_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! STUDENT_PDF evaluates the central Student T PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = Gamma ( (C+1)/2 ) /
!      ( Gamma ( C / 2 ) * Sqrt ( PI * C )
!      * ( 1 + ((X-A)/B)^2/C )^(C + 1/2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = ( x - a ) / b

  pdf = gamma ( 0.5D+00 * ( c + 1.0D+00 ) ) / ( sqrt ( r8_pi * c ) &
    * gamma ( 0.5D+00 * c ) &
    * sqrt ( ( 1.0D+00 + y * y / c ) ** ( 2 * c + 1.0D+00 ) ) )

  return
end

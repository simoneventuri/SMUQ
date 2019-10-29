subroutine logistic_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! LOGISTIC_PDF evaluates the Logistic PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = exp ( ( A - X ) / B ) /
!      ( B * ( 1 + exp ( ( A - X ) / B ) )^2 )
!
!    The Logistic PDF is also known as the Sech-Squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
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
  real ( kind = 8 ) temp
  real ( kind = 8 ) x

  temp = exp ( ( a - x ) / b )

  pdf = temp / ( b * ( 1.0D+00 + temp ) ** 2 )

  return
end

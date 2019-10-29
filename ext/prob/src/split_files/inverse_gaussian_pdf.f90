subroutine inverse_gaussian_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_PDF evaluates the Inverse Gaussian PDF.
!
!  Discussion:
!
!    The Inverse Gaussian PDF is also known as the Wald PDF
!    and the Inverse Normal PDF.
!
!    PDF(A,B;X)
!      = sqrt ( B / ( 2 * PI * X^3 ) )
!        * exp ( - B * ( X - A )^2 / ( 2.0D+00 * A^2 * X ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 < X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
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

  if ( x <= 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = sqrt ( b / ( 2.0D+00 * r8_pi * x ** 3 ) ) * &
      exp ( - b * ( x - a ) ** 2 / ( 2.0D+00 * a * a * x ) )
  end if

  return
end

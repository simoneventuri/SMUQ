subroutine folded_normal_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! FOLDED_NORMAL_PDF evaluates the Folded Normal PDF.
!
!  Discussion:
!
!    The formula for the PDF is:
!
!    PDF(A;X) = sqrt ( 2 / PI ) * ( 1 / B ) * cosh ( A * X / B^2 )
!      * exp ( - 0.5D+00 * ( X^2 + A^2 ) / B^2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
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

  if ( x < 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = sqrt ( 2.0D+00 / r8_pi ) * ( 1.0D+00 / b ) * cosh ( a * x / b ** 2 ) &
      * exp ( - 0.5D+00 * ( x * x + a * a ) / b ** 2 )
  end if

  return
end

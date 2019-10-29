subroutine normal_01_pdf ( x, pdf )

!*****************************************************************************80
!
!! NORMAL_01_PDF evaluates the Normal 01 PDF.
!
!  Discussion:
!
!    The Normal 01 PDF is also called the "Standard Normal" PDF, or
!    the Normal PDF with 0 mean and variance 1.
!
!    PDF(X) = exp ( - 0.5 * X^2 ) / sqrt ( 2 * PI )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  pdf = exp ( -0.5D+00 * x * x ) / sqrt ( 2.0D+00 * r8_pi )

  return
end

subroutine circular_normal_01_pdf ( x, pdf )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_PDF evaluates the Circular Normal 01 PDF.
!
!  Discussion:
!
!    PDF(X) = EXP ( - 0.5D+00 * ( X(1)^2 + X(2)^2 ) ) / ( 2 * PI )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(2), the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(2)

  pdf = exp ( - 0.5D+00 * ( x(1) ** 2 + x(2) ** 2 ) ) / ( 2.0D+00 * r8_pi )

  return
end

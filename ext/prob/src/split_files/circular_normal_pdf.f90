subroutine circular_normal_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_PDF evaluates the Circular Normal PDF.
!
!  Discussion:
!
!    PDF(X) = EXP ( - 0.5D+00 * ( ( (X(1)-A(1))^2 + (X(2)-A(2))^2 ) / B^2 )
!      / ( 2 * PI * B^2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(2), the argument of the PDF.
!
!    Input, real ( kind = 8 ) A(2), a parameter of the PDF, the mean value.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF, the standard deviation.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b
  real ( kind = 8 ) d
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x(2)

  d = ( ( x(1) - a(1) ) ** 2 + ( x(2) - a(2) ) ** 2 ) / b ** 2

  pdf = exp ( - 0.5D+00 * d ) / ( 2.0D+00 * b ** 2 * r8_pi )

  return
end

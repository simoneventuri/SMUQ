subroutine lorentz_pdf ( x, pdf )

!*****************************************************************************80
!
!! LORENTZ_PDF evaluates the Lorentz PDF.
!
!  Discussion:
!
!    PDF(X) = 1 / ( PI * ( 1 + X^2 ) )
!
!    The chief interest of the Lorentz PDF is that it is easily
!    inverted, and can be used to dominate other PDF's in an
!    acceptance/rejection method.
!
!    LORENTZ_PDF(X) = CAUCHY_PDF(0,1;X)
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
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  pdf = 1.0D+00 / ( r8_pi * ( 1.0D+00 + x * x ) )

  return
end

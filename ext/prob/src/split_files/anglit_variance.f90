subroutine anglit_variance ( variance )

!*****************************************************************************80
!
!! ANGLIT_VARIANCE returns the variance of the Anglit PDF.
!
!  Discussion:
!
!    Variance =
!      Integral ( -PI/4 <= X <= PI/4 ) X^2 * sin ( 2 * X + PI / 2 )
!
!    Antiderivative =
!      0.5D+00 * X * sin ( 2 * X + PI / 2 )
!      + ( 0.25 - 0.5D+00 * X^2 ) * cos ( 2 * X + PI / 2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) variance

  variance = 0.0625D+00 * r8_pi * r8_pi - 0.5D+00

  return
end

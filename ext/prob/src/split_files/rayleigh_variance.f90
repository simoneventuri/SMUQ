subroutine rayleigh_variance ( a, variance )

!*****************************************************************************80
!
!! RAYLEIGH_VARIANCE returns the variance of the Rayleigh PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameters of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) variance

  variance = 2.0D+00 * a ** 2 * ( 1.0D+00 - 0.25D+00 * r8_pi )

  return
end

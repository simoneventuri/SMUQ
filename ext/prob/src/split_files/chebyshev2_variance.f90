subroutine chebyshev2_variance ( variance )

!*****************************************************************************80
!
!! CHEBYSHEV2_VARIANCE returns the variance of the Chebyshev2 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2016
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

  variance = r8_pi / 8.0D+00

  return
end

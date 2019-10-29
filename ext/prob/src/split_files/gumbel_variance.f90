subroutine gumbel_variance ( variance )

!*****************************************************************************80
!
!! GUMBEL_VARIANCE returns the variance of the Gumbel PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
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

  variance = r8_pi * r8_pi / 6.0D+00

  return
end

subroutine rayleigh_mean ( a, mean )

!*****************************************************************************80
!
!! RAYLEIGH_MEAN returns the mean of the Rayleigh PDF.
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
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mean
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  mean = a * sqrt ( 0.5D+00 * r8_pi )

  return
end

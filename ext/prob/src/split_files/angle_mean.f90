subroutine angle_mean ( n, mean )

!*****************************************************************************80
!
!! ANGLE_MEAN returns the mean of the Angle PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!    N must be at least 2.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) mean
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  mean = r8_pi / 2.0D+00

  return
end

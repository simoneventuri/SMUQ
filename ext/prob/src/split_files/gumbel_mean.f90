subroutine gumbel_mean ( mean )

!*****************************************************************************80
!
!! GUMBEL_MEAN returns the mean of the Gumbel PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) euler_constant
  real ( kind = 8 ) mean

  mean = euler_constant ( )

  return
end

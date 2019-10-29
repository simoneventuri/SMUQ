subroutine lorentz_mean ( mean )

!*****************************************************************************80
!
!! LORENTZ_MEAN returns the mean of the Lorentz PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) mean

  mean = 0.0D+00

  return
end

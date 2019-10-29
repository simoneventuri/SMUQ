subroutine uniform_01_mean ( mean )

!*****************************************************************************80
!
!! UNIFORM_01_MEAN returns the mean of the Uniform 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
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

  mean = 0.5D+00

  return
end

subroutine chebyshev2_mean ( mean )

!*****************************************************************************80
!
!! CHEBYSHEV2_MEAN returns the mean of the Chebyshev2 PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) mean

  mean = 0.0D+00

  return
end

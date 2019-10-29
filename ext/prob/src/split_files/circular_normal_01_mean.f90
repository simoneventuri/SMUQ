subroutine circular_normal_01_mean ( mean )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_MEAN returns the mean of the Circular Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN(2), the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) mean(2)

  mean(1:2) = 0.0D+00

  return
end

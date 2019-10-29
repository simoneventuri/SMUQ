subroutine disk_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! DISK_MEAN returns the mean of points in a disk.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the disk.
!    The disk is centered at (A,B) and has radius C.
!    0.0 < C.
!
!    Output, real ( kind = 8 ) MEAN(2), the mean value.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean(2)

  mean(1) = a
  mean(2) = b

  return
end

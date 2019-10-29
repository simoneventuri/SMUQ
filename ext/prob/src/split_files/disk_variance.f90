subroutine disk_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! DISK_VARIANCE returns the variance of points in a disk.
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
!    Output, real ( kind = 8 ) VARIANCE, the variance.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) variance

  variance = 0.5D+00 * c * c

  return
end

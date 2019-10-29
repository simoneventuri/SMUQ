subroutine half_normal_mean ( a, b, mean )

!*****************************************************************************80
!
!! HALF_NORMAL_MEAN returns the mean of the Half Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mean
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  mean = a + b * sqrt ( 2.0D+00 / r8_pi )

  return
end

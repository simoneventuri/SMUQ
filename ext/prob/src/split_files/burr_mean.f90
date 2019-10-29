subroutine burr_mean ( a, b, c, d, mean )

!*****************************************************************************80
!
!! BURR_MEAN returns the mean of the Burr PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) mean
  real ( kind = 8 ) r8_beta
  real ( kind = 8 ) ymean

  ymean = d * r8_beta ( d - 1.0D+00 / c, 1.0D+00 + 1.0D+00 / c )

  mean = a + b * ymean

  return
end

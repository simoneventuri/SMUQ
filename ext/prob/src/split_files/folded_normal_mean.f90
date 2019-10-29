subroutine folded_normal_mean ( a, b, mean )

!*****************************************************************************80
!
!! FOLDED_NORMAL_MEAN returns the mean of the Folded Normal PDF.
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
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mean
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00

  a2 = a / b

  call normal_01_cdf ( a2, cdf )

  mean = b * sqrt ( 2.0D+00 / r8_pi ) * exp ( - 0.5D+00 * a2 * a2 ) &
    - a * ( 1.0D+00 - 2.0D+00 * cdf )

  return
end

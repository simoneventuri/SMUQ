subroutine extreme_values_mean ( a, b, mean )

!*****************************************************************************80
!
!! EXTREME_VALUES_MEAN returns the mean of the Extreme Values PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
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
  real ( kind = 8 ) euler_constant
  real ( kind = 8 ) mean

  mean = a + b * euler_constant ( )

  return
end

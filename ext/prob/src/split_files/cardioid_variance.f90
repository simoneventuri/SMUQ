subroutine cardioid_variance ( a, b, variance )

!*****************************************************************************80
!
!! CARDIOID_VARIANCE returns the variance of the Cardioid PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) variance

  variance = 0.0D+00

  return
end

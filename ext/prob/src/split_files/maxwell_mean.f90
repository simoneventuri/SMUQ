subroutine maxwell_mean ( a, mean )

!*****************************************************************************80
!
!! MAXWELL_MEAN returns the mean of the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) MEAN, the mean value.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mean

  mean = sqrt ( 2.0D+00 ) * a * gamma ( 2.0D+00 ) / gamma ( 1.5D+00 )

  return
end

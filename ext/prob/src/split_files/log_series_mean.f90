subroutine log_series_mean ( a, mean )

!*****************************************************************************80
!
!! LOG_SERIES_MEAN returns the mean of the Logarithmic Series PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mean

  mean = - a / ( ( 1.0D+00 - a ) * log ( 1.0D+00 - a ) )

  return
end

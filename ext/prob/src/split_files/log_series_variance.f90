subroutine log_series_variance ( a, variance )

!*****************************************************************************80
!
!! LOG_SERIES_VARIANCE returns the variance of the Logarithmic Series PDF.
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) variance

  alpha = - 1.0D+00 / log ( 1.0D+00 - a )

  variance = a * alpha * ( 1.0D+00 - alpha * a ) / ( 1.0D+00 - a ) ** 2

  return
end

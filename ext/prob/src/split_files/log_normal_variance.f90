subroutine log_normal_variance ( a, b, variance )

!*****************************************************************************80
!
!! LOG_NORMAL_VARIANCE returns the variance of the Lognormal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) variance

  variance = exp ( 2.0D+00 * a + b * b ) * ( exp ( b * b ) - 1.0D+00 )

  return
end

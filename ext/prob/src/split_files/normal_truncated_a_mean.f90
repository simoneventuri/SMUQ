subroutine normal_truncated_a_mean ( mu, s, a, mean )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_MEAN returns the mean of the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviatione of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) alpha_pdf
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) s

  alpha = ( a - mu ) / s

  call normal_01_cdf ( alpha, alpha_cdf )

  call normal_01_pdf ( alpha, alpha_pdf )

  mean = mu + s * alpha_pdf / ( 1.0D+00 - alpha_cdf )

  return
end

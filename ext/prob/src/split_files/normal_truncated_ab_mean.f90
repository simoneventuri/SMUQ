subroutine normal_truncated_ab_mean ( mu, s, a, b, mean )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_MEAN returns the mean of the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
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
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) alpha_pdf
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) beta_pdf
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) s

  alpha = ( a - mu ) / s
  beta = ( b - mu ) / s

  call normal_01_cdf ( alpha, alpha_cdf )
  call normal_01_cdf ( beta, beta_cdf )

  call normal_01_pdf ( alpha, alpha_pdf )
  call normal_01_pdf ( beta, beta_pdf )

  mean = mu + s * ( alpha_pdf - beta_pdf ) / ( beta_cdf - alpha_cdf )

  return
end

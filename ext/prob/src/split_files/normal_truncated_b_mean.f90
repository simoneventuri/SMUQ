subroutine normal_truncated_b_mean ( mu, s, b, mean )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_MEAN returns the mean of the upper truncated Normal PDF.
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
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) beta_pdf
  real ( kind = 8 ) mean
  real ( kind = 8 ) mu
  real ( kind = 8 ) s

  beta = ( b - mu ) / s

  call normal_01_cdf ( beta, beta_cdf )

  call normal_01_pdf ( beta, beta_pdf )

  mean = mu - s * beta_pdf / beta_cdf

  return
end

subroutine normal_truncated_b_variance ( mu, s, b, variance )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_VARIANCE: variance of the upper truncated Normal PDF.
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
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) beta_pdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) s
  real ( kind = 8 ) variance

  beta = ( b - mu ) / s

  call normal_01_pdf ( beta, beta_pdf )

  call normal_01_cdf ( beta, beta_cdf )

  variance = s * s * ( 1.0D+00 &
    - ( beta * beta_pdf ) / beta_cdf &
    - ( beta_pdf / beta_cdf ) ** 2 )

  return
end

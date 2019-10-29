subroutine normal_truncated_b_sample ( mu, s, b, seed, x )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_SAMPLE samples the upper truncated Normal PDF.
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
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) s
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  beta = ( b - mu ) / s

  call normal_01_cdf ( beta, beta_cdf )

  u = r8_uniform_01 ( seed )
  xi_cdf = u * beta_cdf
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + s * xi

  return
end

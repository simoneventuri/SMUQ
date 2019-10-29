subroutine normal_truncated_a_sample ( mu, s, a, seed, x )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_SAMPLE samples the lower truncated Normal PDF.
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
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) s
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  alpha = ( a - mu ) / s

  call normal_01_cdf ( alpha, alpha_cdf )

  u = r8_uniform_01 ( seed )
  xi_cdf = alpha_cdf + u * ( 1.0D+00 - alpha_cdf )
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + s * xi

  return
end

subroutine normal_truncated_b_cdf ( x, mu, s, b, cdf )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_CDF evaluates the upper truncated Normal CDF.
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
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) s
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  beta = ( b - mu ) / s
  xi = ( x - mu ) / s

  call normal_01_cdf ( beta, beta_cdf )
  call normal_01_cdf ( xi, xi_cdf )

  cdf = xi_cdf / beta_cdf

  return
end

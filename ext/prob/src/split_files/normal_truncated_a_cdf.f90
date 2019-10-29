subroutine normal_truncated_a_cdf ( x, mu, s, a, cdf )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_CDF evaluates the lower truncated Normal CDF.
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
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) s
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_cdf

  alpha = ( a - mu ) / s
  xi = ( x - mu ) / s

  call normal_01_cdf ( alpha, alpha_cdf )
  call normal_01_cdf ( xi, xi_cdf )

  cdf = ( xi_cdf - alpha_cdf ) / ( 1.0D+00 - alpha_cdf )

  return
end

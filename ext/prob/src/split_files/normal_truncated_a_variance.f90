subroutine normal_truncated_a_variance ( mu, s, a, variance )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_VARIANCE: variance of the lower truncated Normal PDF.
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) alpha_pdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) s
  real ( kind = 8 ) variance

  alpha = ( a - mu ) / s

  call normal_01_pdf ( alpha, alpha_pdf )

  call normal_01_cdf ( alpha, alpha_cdf )

  variance = s * s * ( 1.0D+00 &
    + ( alpha * alpha_pdf ) / ( 1.0D+00 - alpha_cdf ) &
    - ( alpha_pdf / ( 1.0D+00 - alpha_cdf ) ) ** 2 )

  return
end

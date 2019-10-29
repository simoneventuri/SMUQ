subroutine normal_truncated_a_pdf ( x, mu, s, a, pdf )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_PDF evaluates the lower truncated Normal PDF.
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
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) alpha_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) s
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_pdf

  alpha = ( a - mu ) / s
  xi = ( x - mu ) / s

  call normal_01_cdf ( alpha, alpha_cdf )
  call normal_01_pdf ( xi, xi_pdf )

  pdf = xi_pdf / ( 1.0D+00 - alpha_cdf ) / s

  return
end

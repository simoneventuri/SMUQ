subroutine normal_truncated_b_pdf ( x, mu, s, b, pdf )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_PDF evaluates the upper truncated Normal PDF.
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
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_cdf
  real ( kind = 8 ) mu
  real ( kind = 8 ) pdf
  real ( kind = 8 ) s
  real ( kind = 8 ) x
  real ( kind = 8 ) xi
  real ( kind = 8 ) xi_pdf

  beta = ( b - mu ) / s
  xi = ( x - mu ) / s

  call normal_01_cdf ( beta, beta_cdf )
  call normal_01_pdf ( xi, xi_pdf )

  pdf = xi_pdf / beta_cdf / s

  return
end

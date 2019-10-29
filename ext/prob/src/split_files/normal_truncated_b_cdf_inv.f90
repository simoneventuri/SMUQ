subroutine normal_truncated_b_cdf_inv ( cdf, mu, s, b, x )

!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_CDF_INV inverts the upper truncated Normal CDF.
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
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
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

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NORMAL_TRUNCATED_B_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  beta = ( b - mu ) / s

  call normal_01_cdf ( beta, beta_cdf )

  xi_cdf = beta_cdf * cdf
  call normal_01_cdf_inv ( xi_cdf, xi )

  x = mu + s * xi

  return
end

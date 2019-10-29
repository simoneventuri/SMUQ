subroutine beta_binomial_sample ( a, b, c, seed, x )

!*****************************************************************************80
!
!! BETA_BINOMIAL_SAMPLE samples the Beta Binomial CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x

  cdf = r8_uniform_01 ( seed )

  call beta_binomial_cdf_inv ( cdf, a, b, c, x )

  return
end

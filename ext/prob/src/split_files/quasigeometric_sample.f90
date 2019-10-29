subroutine quasigeometric_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! QUASIGEOMETRIC_SAMPLE samples the Quasigeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x

  cdf = r8_uniform_01 ( seed )

  call quasigeometric_cdf_inv ( cdf, a, b, x )

  return
end

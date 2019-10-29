function chebyshev1_sample ( seed )

!*****************************************************************************80
!
!! CHEBYSHEV1_SAMPLE samples the Chebyshev1 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) CHEBYSHEV1_SAMPLE, a random value.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) chebyshev1_sample
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  cdf = r8_uniform_01 ( seed )

  call chebyshev1_cdf_inv ( cdf, x )

  chebyshev1_sample = x

  return
end

subroutine uniform_discrete_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! UNIFORM_DISCRETE_SAMPLE samples the Uniform discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x

  cdf = r8_uniform_01 ( seed )

  call uniform_discrete_cdf_inv ( cdf, a, b, x )

  return
end

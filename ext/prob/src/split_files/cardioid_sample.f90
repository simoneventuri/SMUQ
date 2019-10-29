subroutine cardioid_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! CARDIOID_SAMPLE samples the Cardioid PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!    A - PI <= X <= A + PI.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  cdf = r8_uniform_01 ( seed )

  call cardioid_cdf_inv ( cdf, a, b, x )

  return
end

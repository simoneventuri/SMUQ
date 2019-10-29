subroutine burr_sample ( a, b, c, d, seed, x )

!*****************************************************************************80
!
!! BURR_SAMPLE samples the Burr PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) d
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  cdf = r8_uniform_01 ( seed )

  call burr_cdf_inv ( cdf, a, b, c, d, x )

  return
end

subroutine exponential_01_sample ( seed, x )

!*****************************************************************************80
!
!! EXPONENTIAL_01_SAMPLE samples the Exponential PDF with parameter 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  cdf = r8_uniform_01 ( seed )

  x = - log ( 1.0D+00 - cdf )

  return
end

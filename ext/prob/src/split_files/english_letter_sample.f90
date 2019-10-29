subroutine english_letter_sample ( seed, c )

!*****************************************************************************80
!
!! ENGLISH_LETTER_SAMPLE samples the English Letter PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!    Output, character C, a sample of the PDF.
!
!    Output, integer ( kind = 4 ) SEED, an updated seed for the random
!    number generator.
!
  implicit none

  character c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed

  cdf = r8_uniform_01 ( seed )

  call english_letter_cdf_inv ( cdf, c )

  return
end

subroutine discrete_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! DISCRETE_SAMPLE samples the Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) b_sum
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x

  b_sum = sum ( b(1:a) )

  cdf = r8_uniform_01 ( seed )

  call discrete_cdf_inv ( cdf, a, b, x )

  return
end

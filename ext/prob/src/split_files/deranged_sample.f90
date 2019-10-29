subroutine deranged_sample ( a, seed, x )

!*****************************************************************************80
!
!! DERANGED_SAMPLE samples the Deranged PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) cdf
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x

  cdf = r8_uniform_01 ( seed )

  call deranged_cdf_inv ( cdf, a, x )

  return
end

subroutine normal_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! NORMAL_SAMPLE samples the Normal PDF.
!
!  Discussion:
!
!    The Box-Muller method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  call normal_01_sample ( seed, x )

  x = a + b * x

  return
end

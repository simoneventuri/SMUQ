subroutine chi_sample ( a, b, c, seed, x )

!*****************************************************************************80
!
!! CHI_SAMPLE samples the Chi PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
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
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  call chi_square_sample ( c, seed, x )

  x = a + b * sqrt ( x )

  return
end

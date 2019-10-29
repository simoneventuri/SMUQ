subroutine pearson_05_sample ( a, b, c, seed, x )

!*****************************************************************************80
!
!! PEARSON_05_SAMPLE samples the Pearson 5 PDF.
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
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) b2
  real ( kind = 8 ) c
  real ( kind = 8 ) c2
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  a2 = 0.0D+00
  b2 = b
  c2 = 1.0D+00 / a

  call gamma_sample ( a2, b2, c2, seed, x2 )

  x = c + 1.0D+00 / x2

  return
end

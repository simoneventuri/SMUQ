subroutine chi_square_noncentral_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_SAMPLE samples the noncentral Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the parameter of the PDF.
!    1.0D+00 <= A.
!
!    Input, real ( kind = 8 ) B, the noncentrality parameter of the PDF.
!    0.0 <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a1
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) b2
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2

  a1 = a - 1.0D+00

  call chi_square_sample ( a1, seed, x1 )

  a2 = sqrt ( b )
  b2 = 1.0D+00
  call normal_sample ( a2, b2, seed, x2 )

  x = x1 + x2 * x2

  return
end

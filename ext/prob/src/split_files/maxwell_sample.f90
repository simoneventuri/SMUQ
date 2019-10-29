subroutine maxwell_sample ( a, seed, x )

!*****************************************************************************80
!
!! MAXWELL_SAMPLE samples the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x

  a2 = 3.0D+00
  call chi_square_sample ( a2, seed, x )

  x = a * sqrt ( x )

  return
end

subroutine f_sample ( m, n, seed, x )

!*****************************************************************************80
!
!! F_SAMPLE samples the F central PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) xm
  real ( kind = 8 ) xn

  a = real ( m, kind = 8 )
  call chi_square_sample ( a, seed, xm )

  a = real ( n, kind = 8 )
  call chi_square_sample ( a, seed, xn )

  x = real ( n, kind = 8 ) * xm / ( real ( m, kind = 8 ) * xn )

  return
end

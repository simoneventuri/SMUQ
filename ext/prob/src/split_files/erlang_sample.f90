subroutine erlang_sample ( a, b, c, seed, x )

!*****************************************************************************80
!
!! ERLANG_SAMPLE samples the Erlang PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b
  real ( kind = 8 ) b2
  integer ( kind = 4 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  a2 = 0.0D+00
  b2 = b
  x = a
  do i = 1, c
    call exponential_sample ( a2, b2, seed, x2 )
    x = x + x2
  end do

  return
end

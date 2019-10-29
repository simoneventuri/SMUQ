subroutine hypergeometric_sample ( n, m, l, seed, x )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_SAMPLE samples the Hypergeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 165.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c1_log
  real ( kind = 8 ) c2_log
  real ( kind = 8 ) i4_choose_log
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  integer ( kind = 4 ) x

  c1_log = i4_choose_log ( l - m, n )
  c2_log = i4_choose_log ( l, n )

  a = exp ( c1_log - c2_log )
  b = a

  u = r8_uniform_01 ( seed )

  x = 0

  do while ( a < u )

    b = b * real ( ( m - x ) * ( n - x ), kind = 8 ) &
      / real ( ( x + 1 ) * ( l - m - n + x + 1 ), kind = 8 )

    a = a + b

    x = x + 1

  end do

  return
end

subroutine hypergeometric_cdf ( x, n, m, l, cdf )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_CDF evaluates the Hypergeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
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
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) c1_log
  real ( kind = 8 ) c2_log
  real ( kind = 8 ) i4_choose_log
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  integer ( kind = 4 ) x2

  c1_log = i4_choose_log ( l - m, n )
  c2_log = i4_choose_log ( l, n )

  pdf = exp ( c1_log - c2_log )
  cdf = pdf

  do x2 = 0, x - 1

    pdf = pdf * real ( ( m - x2 ) * ( n - x2 ), kind = 8 ) &
      / real ( ( x2 + 1 ) * ( l - m - n + x2 + 1 ), kind = 8 )

    cdf = cdf + pdf

  end do

  return
end

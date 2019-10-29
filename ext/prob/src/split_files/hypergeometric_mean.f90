subroutine hypergeometric_mean ( n, m, l, mean )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_MEAN returns the mean of the Hypergeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls in the population.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  real ( kind = 8 ) mean
  integer ( kind = 4 ) n

  mean = real ( n * m, kind = 8 ) / real ( l, kind = 8 )

  return
end

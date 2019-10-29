subroutine hypergeometric_variance ( n, m, l, variance )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_VARIANCE returns the variance of the Hypergeometric PDF.
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
!    Input, integer ( kind = 4 ) M, the number of white balls.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) variance

  variance = real ( n * m * ( l - m ) * ( l - n ), kind = 8 ) &
    / real ( l * l * ( l - 1 ), kind = 8 )

  return
end

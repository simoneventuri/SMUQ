subroutine runs_variance ( m, n, variance )

!*****************************************************************************80
!
!! RUNS_VARIANCE returns the variance of the Runs PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) variance

  variance = real ( 2 * m * n * ( 2 * m * n - m - n ), kind = 8 ) &
           / real ( ( m + n ) * ( m + n ) * ( m + n - 1 ), kind = 8 )

  return
end

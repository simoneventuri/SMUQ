subroutine runs_mean ( m, n, mean )

!*****************************************************************************80
!
!! RUNS_MEAN returns the mean of the Runs PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  integer ( kind = 4 ) m
  real ( kind = 8 ) mean
  integer ( kind = 4 ) n

  mean = real ( m + 2 * m * n + n, kind = 8 ) &
       / real ( m             + n, kind = 8 )

  return
end

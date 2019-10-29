subroutine runs_sample ( m, n, seed, r )

!*****************************************************************************80
!
!! RUNS_SAMPLE samples the Runs PDF.
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
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) R, the number of runs.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m+n)
  integer ( kind = 4 ) r
  integer ( kind = 4 ) seed

  call runs_simulate ( m, n, seed, a )

  call i4vec_run_count ( m+n, a, r )

  return
end

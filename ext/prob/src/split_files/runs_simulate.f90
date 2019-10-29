subroutine runs_simulate ( m, n, seed, a )

!*****************************************************************************80
!
!! RUNS_SIMULATE simulates a case governed by the Runs PDF.
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
!    Output, integer ( kind = 4 ) A(M+N), a sequence of M 0's and N 1's chosen
!    uniformly at random.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m+n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed

  a(1:m) = 0
  a(m+1:m+n) = 1

  do i = 1, m + n - 1

    j = i4_uniform_ab ( i, m + n, seed )

    k    = a(i)
    a(i) = a(j)
    a(j) = k

  end do

  return
end

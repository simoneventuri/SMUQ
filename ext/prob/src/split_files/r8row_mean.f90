subroutine r8row_mean ( m, n, x, mean )

!*****************************************************************************80
!
!! R8ROW_MEAN returns the means of rows of an R8ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) X(M,N), the array whose row means are desired.
!
!    Output, real ( kind = 8 ) MEAN(M), the means, or averages,
!    of the rows of X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean(m)
  real ( kind = 8 ) x(m,n)

  do i = 1, m
    mean(i) = sum ( x(i,1:n) ) / real ( n )
  end do

  return
end

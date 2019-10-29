subroutine r8row_variance ( m, n, x, variance )

!*****************************************************************************80
!
!! R8ROW_VARIANCE returns the variances of the rows of an R8ROW.
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
!    Output, real ( kind = 8 ) VARIANCE(M), the variances of the rows of X.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance(m)
  real ( kind = 8 ) x(m,n)

  do i = 1, m

    mean = sum ( x(i,1:n) ) / real ( n, kind = 8 )

    variance(i) = sum ( ( x(i,1:n) - mean ) ** 2 )

    if ( 1 < n ) then
      variance(i) = variance(i) / real ( n - 1, kind = 8 )
    else
      variance(i) = 0.0D+00
    end if

  end do

  return
end

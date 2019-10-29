subroutine i4row_mean ( m, n, a, mean )

!*****************************************************************************80
!
!! I4ROW_MEAN returns the means of the rows of an I4ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of data.
!
!    Input, integer ( kind = 4 ) A(M,N), the array.
!
!    Output, real ( kind = 8 ) MEAN(M), the mean of each row.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean(m)

  do i = 1, m
    mean(i) = sum ( a(i,1:n) ) / real ( n, kind = 8 )
  end do

  return
end

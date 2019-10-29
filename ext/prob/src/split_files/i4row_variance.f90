subroutine i4row_variance ( m, n, a, variance )

!*****************************************************************************80
!
!! I4ROW_VARIANCE returns the variances of the rows of an I4ROW.
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
!    Output, real ( kind = 8 ) VARIANCE(M), the variance of each row.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance(m)

  do i = 1, m

    mean = real ( sum ( a(i,1:n) ), kind = 8 ) / real ( n, kind = 8 )

    variance(i) = 0.0D+00
    do j = 1, n
      variance(i) = variance(i) + ( real ( a(i,j), kind = 8 ) - mean ) ** 2
    end do

    if ( 1 < n ) then
      variance(i) = variance(i) / real ( n - 1, kind = 8 )
    else
      variance(i) = 0.0D+00
    end if

  end do

  return
end

subroutine r8row_min ( m, n, a, amin )

!*****************************************************************************80
!
!! R8ROW_MIN returns the minimums of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Example:
!
!    A =
!      1  2  3
!      2  6  7
!
!    MIN =
!      1
!      2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A(M,N), the array to be examined.
!
!    Output, real ( kind = 8 ) AMIN(M), the minimums of the rows.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  real ( kind = 8 ) amin(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do i = 1, m

    amin(i) = a(i,1)
    do j = 2, n
      if ( a(i,j) < amin(i) ) then
        amin(i) = a(i,j)
      end if
    end do

  end do

  return
end

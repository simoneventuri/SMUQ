subroutine i4vec_unique_count ( n, a, unique_num )

!*****************************************************************************80
!
!! I4VEC_UNIQUE_COUNT counts the unique elements in an unsorted I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    Because the array is unsorted, this algorithm is O(N^2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, integer ( kind = 4 ) A(N), the unsorted array to examine.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) unique_num

  unique_num = 0

  do i = 1, n

    unique_num = unique_num + 1

    do j = 1, i - 1

      if ( a(i) == a(j) ) then
        unique_num = unique_num - 1
        exit
      end if

    end do

  end do

  return
end

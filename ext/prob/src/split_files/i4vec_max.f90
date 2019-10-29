subroutine i4vec_max ( n, a, amax )

!*****************************************************************************80
!
!! I4VEC_MAX computes the maximum element of an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, integer ( kind = 4 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) AMAX, the value of the largest entry.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) amax

  amax = maxval ( a(1:n) )

  return
end

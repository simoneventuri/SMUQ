subroutine r8vec_unit_sum ( n, a )

!*****************************************************************************80
!
!! R8VEC_UNIT_SUM normalizes an R8VEC to have unit sum.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, real A(N), the vector to be normalized.  On output,
!    the entries of A should have unit sum.  However, if the input vector
!    has zero sum, the routine halts.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_sum

  a_sum = sum ( a(1:n) )

  if ( a_sum == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIT_SUM - Fatal error!'
    write ( *, '(a)' ) '  The vector entries sum to 0.'
    stop 1
  end if

  a(1:n) = a(1:n) / a_sum

  return
end

subroutine i4vec_run_count ( n, a, run_count )

!*****************************************************************************80
!
!! I4VEC_RUN_COUNT counts runs of equal values in an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of integer values.
!
!    A run is a sequence of equal values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be examined.
!
!    Output, integer ( kind = 4 ) RUN_COUNT, the number of runs.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) run_count
  integer ( kind = 4 ) test

  run_count = 0

  if ( n < 1 ) then
    return
  end if

  test = 0

  do i = 1, n

    if ( i == 1 .or. a(i) /= test ) then
      run_count = run_count + 1
      test = a(i)
    end if

  end do

  return
end

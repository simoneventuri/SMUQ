function i4_is_power_of_10 ( n )

!*****************************************************************************80
!
!! I4_IS_POWER_OF_10 reports whether an I4 is a power of 10.
!
!  Discussion:
!
!    The powers of 10 are 1, 10, 100, 1000, 10000, and so on.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the integer to be tested.
!
!    Output, logical ( kind = 4 ) I4_IS_POWER_OF_10, TRUE if N is a power of 10.
!
  implicit none

  logical ( kind = 4 ) i4_is_power_of_10
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_copy

  n_copy = n
  i4_is_power_of_10 = .false.

  if ( n_copy <= 0 ) then
    return
  end if

  do while ( 1 < n_copy )

    if ( mod ( n_copy, 10 ) /= 0 ) then
      return
    end if

    n_copy = n_copy / 10

  end do

  i4_is_power_of_10 = .true.

  return
end

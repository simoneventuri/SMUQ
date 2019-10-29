subroutine birthday_sample ( n, seed, value )

!*****************************************************************************80
!
!! BIRTHDAY_SAMPLE samples the Birthday Concurrence PDF.
!
!  Discussion:
!
!    The probability is the probability that the N-th person is the
!    first one to match a birthday with someone earlier.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of people whose birthdays have been
!    disclosed.
!
!    Input, integer SEED, a seed for the random number generator.
!
!    Output, integer VALUE,
!    * 1 if the first N-1 people had distinct
!      birthdays, but person N had a birthday in common with a previous person,
!    * 0 otherwise.
!
!    Output, integer SEED, a seed for the random number generator.
!
  implicit none

  integer ( kind = 4 ), allocatable :: b(:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) u1
  integer ( kind = 4 ) u2
  integer ( kind = 4 ) value

  if ( n < 1 ) then
    value = 0
    return
  end if
!
!  Choose N birthdays at random.
!
  allocate ( b(1:n) )

  call i4vec_uniform_ab ( n, 1, 365, seed, b )
!
!  Are the first N-1 birthdays unique?
!
  call i4vec_unique_count ( n - 1, b(1:n-1), u1 )

  if ( u1 < n - 1 ) then
    value = 0
    return
  end if
!
!  Does the N-th birthday match an earlier one?
!
  call i4vec_unique_count ( n, b(1:n), u2 )

  if ( u2 == n - 1 ) then
    value = 1
  else
    value = 0
  end if

  deallocate ( b )

  return
end

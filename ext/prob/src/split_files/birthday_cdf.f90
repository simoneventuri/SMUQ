subroutine birthday_cdf ( n, cdf )

!*****************************************************************************80
!
!! BIRTHDAY_CDF returns the Birthday Concurrence CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of people whose birthdays have
!    been disclosed.
!
!    Output, real ( kind = 8 ) CDF, the probability that at least
!    two of the N people have matching birthays.
!
  implicit none

  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n

  if ( n < 1 ) then
    cdf = 0.0D+00
    return
  else if ( 365 < n ) then
    cdf = 1.0D+00
    return
  end if
!
!  Compute the probability that N people have distinct birthdays.
!
  cdf = 1.0D+00
  do i = 1, n
    cdf = cdf * real ( 365 + 1 - i, kind = 8 ) / 365.0D+00
  end do
!
!  Compute the probability that it is NOT the case that N people
!  have distinct birthdays.  This is the cumulative probability
!  that person 2 matches person 1, or person 3 matches 1 or 2,
!  etc.
!
  cdf = 1.0D+00 - cdf

  return
end

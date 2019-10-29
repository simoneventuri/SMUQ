subroutine birthday_pdf ( n, pdf )

!*****************************************************************************80
!
!! BIRTHDAY_PDF returns the Birthday Concurrence PDF.
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
!    Output, real ( kind = 8 ) PDF, the probability that the N-th person
!    is the first to match a birthday with someone earlier.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf

  if ( n < 1 .or. 365 < n ) then
    pdf = 0.0D+00
    return
  end if

  pdf = 1.0D+00
!
!  Compute the probability that N-1 people have distinct birthdays.
!
  do i = 1, n-1
    pdf = pdf * real ( 365 + 1 - i, kind = 8 ) / 365.0D+00
  end do
!
!  Compute the probability that person N has one of those N-1 birthdays.
!
  pdf = pdf * real ( n - 1, kind = 8 ) / 365.0D+00

  return
end

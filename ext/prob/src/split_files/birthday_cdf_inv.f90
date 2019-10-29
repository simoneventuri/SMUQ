subroutine birthday_cdf_inv ( cdf, n )

!*****************************************************************************80
!
!! BIRTHDAY_CDF_INV inverts the Birthday Concurrence CDF.
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
!    Input, real ( kind = 8 ) CDF, the probability that at least
!    two of the N people have matching birthays.
!
!    Output, integer ( kind = 4 ) N, the corresponding number of people whose
!    birthdays need to be disclosed.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf_not
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n

  if ( cdf <= 0.0D+00 ) then
    n = 1
    return
  else if ( 1.0D+00 <= cdf ) then
    n = 365
    return
  end if
!
!  Compute the probability that N people have distinct birthdays.
!
  cdf_not = 1.0D+00

  do i = 1, 365
    cdf_not = cdf_not * real ( 365 + 1 - i, kind = 8 ) / 365.0D+00
    if ( cdf <= 1.0D+00 - cdf_not ) then
      n = i
      return
    end if
  end do

  n = 365

  return
end

subroutine poisson_cdf_inv ( cdf, a, x )

!*****************************************************************************80
!
!! POISSON_CDF_INV inverts the Poisson CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, a value of the CDF.
!    0 <= CDF < 1.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) last
  real ( kind = 8 ) new
  real ( kind = 8 ) sum2
  real ( kind = 8 ) sumold
  integer ( kind = 4 ) x
  integer ( kind = 4 ), parameter :: xmax = 100

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POISSON_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if
!
!  Now simply start at X = 0, and find the first value for which
!  CDF(X-1) <= CDF <= CDF(X).
!
  sum2 = 0.0D+00

  do i = 0, xmax

    sumold = sum2

    if ( i == 0 ) then
      new = exp ( - a )
      sum2 = new
    else
      last = new
      new = last * a / real ( i, kind = 8 )
      sum2 = sum2 + new
    end if

    if ( sumold <= cdf .and. cdf <= sum2 ) then
      x = i
      return
    end if

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'POISSON_CDF_INV - Warning!'
  write ( *, '(a,i8)' ) '  Exceeded XMAX = ', xmax

  x = xmax

  return
end

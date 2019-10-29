subroutine log_series_cdf_inv ( cdf, a, x )

!*****************************************************************************80
!
!! LOG_SERIES_CDF_INV inverts the Logarithmic Series CDF.
!
!  Discussion:
!
!    Simple summation is used.  The only protection against an
!    infinite loop caused by roundoff is that X cannot be larger
!    than 1000.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) X, the argument of the CDF for which
!    CDF(X-1) <= CDF <= CDF(X).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  integer ( kind = 4 ), parameter :: xmax = 1000

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LOG_SERIES_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  cdf2 = 0.0D+00
  x = 1

  do while ( cdf2 < cdf .and. x < xmax )

    if ( x == 1 ) then
      pdf = - a / log ( 1.0D+00 - a )
    else
      pdf = real ( x - 1, kind = 8 ) * a * pdf / real ( x, kind = 8 )
    end if

    cdf2 = cdf2 + pdf

    x = x + 1

  end do

  return
end

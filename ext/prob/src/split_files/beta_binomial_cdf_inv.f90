subroutine beta_binomial_cdf_inv ( cdf, a, b, c, x )

!*****************************************************************************80
!
!! BETA_BINOMIAL_CDF_INV inverts the Beta Binomial CDF.
!
!  Discussion:
!
!    A simple discrete approach is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, integer ( kind = 4 ) X, the smallest X whose cumulative density
!    function is greater than or equal to CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cum
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_beta
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_BINOMIAL_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  cum = 0.0D+00

  do y = 0, c

    pdf = r8_beta ( a + real ( y, kind = 8 ), &
      b + real ( c - y, kind = 8 ) ) / ( real ( c + 1, kind = 8 ) &
      * r8_beta ( real ( y + 1, kind = 8 ), &
      real ( c - y + 1, kind = 8 ) ) * r8_beta ( a, b ) )

    cum = cum + pdf

    if ( cdf <= cum ) then
      x = y
      return
    end if

  end do

  x = c

  return
end

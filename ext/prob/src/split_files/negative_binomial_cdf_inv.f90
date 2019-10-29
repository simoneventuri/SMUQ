subroutine negative_binomial_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CDF_INV inverts the Negative Binomial CDF.
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
!    06 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, integer ( kind = 4 ) X, the smallest X whose cumulative density
!    function is greater than or equal to CDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cum
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  integer ( kind = 4 ), parameter :: x_max = 1000

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NEGATIVE_BINOMIAL_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if


  cum = 0.0D+00

  x = a

  do

    call negative_binomial_pdf ( x, a, b, pdf )

    cum = cum + pdf

    if ( cdf <= cum .or. x_max <= x ) then
      exit
    end if

    x = x + 1

  end do

  return
end

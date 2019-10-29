subroutine gompertz_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! GOMPERTZ_CDF evaluates the Gompertz CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Johnson, Samuel Kotz, Balakrishnan,
!    Continuous Univariate Distributions, second edition,
!    Wiley, 1994,
!    QA273.6.J6
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1 < A, 0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then
    cdf = 0.0D+00
  else
    cdf = 1.0D+00 - exp ( - b * ( a**x - 1.0D+00 ) / log ( a ) )
  end if

  return
end

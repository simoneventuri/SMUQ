subroutine weibull_discrete_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! WEIBULL_DISCRETE_CDF evaluates the Discrete Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!    0 <= X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A <= 1.0D+00,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x

  if ( x < 0 ) then
    cdf = 0.0D+00
  else
    cdf = 1.0D+00 - ( 1.0D+00 - a ) ** ( ( x + 1 ) ** b )
  end if

  return
end

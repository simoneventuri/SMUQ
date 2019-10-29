subroutine genlogistic_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! GENLOGISTIC_CDF evaluates the Generalized Logistic CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  y = ( x - a ) / b

  cdf = 1.0D+00 / ( 1.0D+00 + exp ( - y ) ) ** c

  return
end

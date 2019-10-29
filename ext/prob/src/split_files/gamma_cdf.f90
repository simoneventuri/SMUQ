subroutine gamma_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! GAMMA_CDF evaluates the Gamma CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
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
  real ( kind = 8 ) p2
  real ( kind = 8 ) r8_gamma_inc
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  x2 = ( x - a ) / b
  p2 = c

  cdf = r8_gamma_inc ( p2, x2 )

  return
end

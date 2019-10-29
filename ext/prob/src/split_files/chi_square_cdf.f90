subroutine chi_square_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! CHI_SQUARE_CDF evaluates the Chi squared CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value of the random deviate.
!
!    Input, real ( kind = 8 ) A, the parameter of the distribution, usually
!    the number of degrees of freedom.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b2
  real ( kind = 8 ) c2
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  x2 = 0.5D+00 * x

  a2 = 0.0D+00
  b2 = 1.0D+00
  c2 = 0.5D+00 * a

  call gamma_cdf ( x2, a2, b2, c2, cdf )

  return
end

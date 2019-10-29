subroutine chi_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! CHI_CDF evaluates the Chi CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
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
  real ( kind = 8 ) y

  if ( x <= a ) then

    cdf = 0.0D+00

  else

    y = ( x - a ) / b
    x2 = 0.5D+00 * y * y
    p2 = 0.5D+00 * c

    cdf = r8_gamma_inc ( p2, x2 )

  end if

  return
end

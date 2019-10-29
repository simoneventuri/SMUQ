subroutine weibull_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! WEIBULL_VARIANCE returns the variance of the Weibull PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) g1
  real ( kind = 8 ) g2
  real ( kind = 8 ) variance

  g1 = gamma ( ( c + 2.0D+00 ) / c )
  g2 = gamma ( ( c + 1.0D+00 ) / c )

  variance = b * b * ( g1 - g2 * g2 )

  return
end

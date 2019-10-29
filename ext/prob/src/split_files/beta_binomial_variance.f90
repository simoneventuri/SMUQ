subroutine beta_binomial_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! BETA_BINOMIAL_VARIANCE returns the variance of the Beta Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) variance

  variance = ( real ( c, kind = 8 ) * a * b ) &
    * ( a + b + real ( c, kind = 8 ) ) &
    / ( ( a + b ) ** 2 * ( a + b + 1.0D+00 ) )

  return
end

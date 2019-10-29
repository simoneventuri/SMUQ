subroutine genlogistic_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! GENLOGISTIC_VARIANCE returns the variance of the Generalized Logistic PDF.
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
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) trigamma
  real ( kind = 8 ) variance

  variance = b * b * ( r8_pi * r8_pi / 6.0D+00 + trigamma ( c ) )

  return
end

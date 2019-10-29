subroutine genlogistic_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! GENLOGISTIC_MEAN returns the mean of the Generalized Logistic PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) digamma
  real ( kind = 8 ) euler_constant
  real ( kind = 8 ) mean

  mean = a + b * ( euler_constant ( ) + digamma ( c ) )

  return
end

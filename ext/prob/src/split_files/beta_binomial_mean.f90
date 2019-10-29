subroutine beta_binomial_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! BETA_BINOMIAL_MEAN returns the mean of the Beta Binomial PDF.
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
!    0 <= N.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) mean

  mean = real ( c, kind = 8 ) * a / ( a + b )

  return
end

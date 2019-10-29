subroutine beta_variance ( a, b, variance )

!*****************************************************************************80
!
!! BETA_VARIANCE returns the variance of the Beta PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) variance

  variance = ( a * b ) / ( ( a + b ) ** 2 * ( 1.0D+00 + a + b ) )

  return
end

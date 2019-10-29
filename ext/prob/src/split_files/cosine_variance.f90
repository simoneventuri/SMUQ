subroutine cosine_variance ( a, b, variance )

!*****************************************************************************80
!
!! COSINE_VARIANCE returns the variance of the Cosine PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) variance

  variance = ( r8_pi * r8_pi / 3.0D+00 - 2.0D+00 ) * b * b

  return
end

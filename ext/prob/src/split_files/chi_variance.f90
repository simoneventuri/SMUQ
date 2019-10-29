subroutine chi_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! CHI_VARIANCE returns the variance of the Chi PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) variance

  variance = b * b * ( c - 2.0D+00 * &
    ( gamma ( 0.5D+00 * ( c + 1.0D+00 ) ) / gamma ( 0.5D+00 * c ) ) ** 2 )

  return
end

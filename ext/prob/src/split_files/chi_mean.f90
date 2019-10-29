subroutine chi_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! CHI_MEAN returns the mean of the Chi PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean value.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean

  mean = a + sqrt ( 2.0D+00 ) * b * gamma ( 0.5D+00 * ( c + 1.0D+00 ) ) &
    / gamma ( 0.5D+00 * c )

  return
end

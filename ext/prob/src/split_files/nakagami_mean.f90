subroutine nakagami_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! NAKAGAMI_MEAN returns the mean of the Nakagami PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B
!    0.0 < C
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean

  mean = a + b * gamma ( c + 0.5D+00 ) / ( sqrt ( c ) * gamma ( c ) )

  return
end

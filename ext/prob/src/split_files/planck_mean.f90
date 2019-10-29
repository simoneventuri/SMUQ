subroutine planck_mean ( a, b, mean )

!*****************************************************************************80
!
!! PLANCK_MEAN returns the mean of the Planck PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mean
  real ( kind = 8 ) r8_zeta

  mean = ( b + 1.0D+00 ) * r8_zeta ( b + 2.0D+00 ) / r8_zeta ( b + 1.0D+00 )

  return
end

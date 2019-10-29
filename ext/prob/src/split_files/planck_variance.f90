subroutine planck_variance ( a, b, variance )

!*****************************************************************************80
!
!! PLANCK_VARIANCE returns the variance of the Planck PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mean
  real ( kind = 8 ) r8_zeta
  real ( kind = 8 ) variance

  call planck_mean ( a, b, mean )

  variance = ( b + 1.0D+00 ) * ( b + 2.0D+00 ) &
    * r8_zeta ( b + 3.0D+00 ) / r8_zeta ( b + 1.0D+00 ) - mean * mean

  return
end

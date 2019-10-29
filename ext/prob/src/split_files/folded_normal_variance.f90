subroutine folded_normal_variance ( a, b, variance )

!*****************************************************************************80
!
!! FOLDED_NORMAL_VARIANCE returns the variance of the Folded Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance

  call folded_normal_mean ( a, b, mean )

  variance = a * a + b * b - mean * mean

  return
end

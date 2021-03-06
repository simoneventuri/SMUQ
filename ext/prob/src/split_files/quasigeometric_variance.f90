subroutine quasigeometric_variance ( a, b, variance )

!*****************************************************************************80
!
!! QUASIGEOMETRIC_VARIANCE returns the variance of the Quasigeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) variance

  variance = ( 1.0D+00 - a ) * ( a + b ) / ( 1.0D+00 - b ) / ( 1.0D+00 - b )

  return
end

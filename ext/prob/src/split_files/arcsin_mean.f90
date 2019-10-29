subroutine arcsin_mean ( a, mean )

!*****************************************************************************80
!
!! ARCSIN_MEAN returns the mean of the Arcsin PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) mean

  mean = 0.0D+00

  return
end

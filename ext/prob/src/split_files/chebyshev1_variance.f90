subroutine chebyshev1_variance ( variance )

!*****************************************************************************80
!
!! CHEBYSHEV1_VARIANCE returns the variance of the Chebyshev1 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) variance

  variance = 0.5D+00

  return
end

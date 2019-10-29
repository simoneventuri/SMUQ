subroutine uniform_01_variance ( variance )

!*****************************************************************************80
!
!! UNIFORM_01_VARIANCE returns the variance of the Uniform 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
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

  variance = 1.0D+00 / 12.0D+00

  return
end

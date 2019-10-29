subroutine circular_normal_01_variance ( variance )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_VARIANCE: variance of the Circular Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE(2), the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) variance(2)

  variance(1) = 1.0D+00
  variance(2) = 1.0D+00

  return
end

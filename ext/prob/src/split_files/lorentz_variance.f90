subroutine lorentz_variance ( variance )

!*****************************************************************************80
!
!! LORENTZ_VARIANCE returns the variance of the Lorentz PDF.
!
!  Discussion:
!
!    The variance of the Lorentz PDF is not well defined.  This routine
!    is made available for completeness only, and simply returns
!    a "very large" number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) variance

  variance = huge ( variance )

  return
end

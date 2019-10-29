subroutine cauchy_variance ( a, b, variance )

!*****************************************************************************80
!
!! CAUCHY_VARIANCE returns the variance of the Cauchy PDF.
!
!  Discussion:
!
!    The variance of the Cauchy PDF is not well defined.  This routine
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
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) variance

  variance = huge ( variance )

  return
end

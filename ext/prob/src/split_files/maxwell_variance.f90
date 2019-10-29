subroutine maxwell_variance ( a, variance )

!*****************************************************************************80
!
!! MAXWELL_VARIANCE returns the variance of the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) variance

  variance = a * a * ( 3.0D+00 - 2.0D+00 &
    * ( gamma ( 2.0D+00 ) / gamma ( 1.5D+00 ) ) ** 2 )

  return
end

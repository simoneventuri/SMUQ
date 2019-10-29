subroutine circular_normal_variance ( a, b, variance )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_VARIANCE returns the variance of the Circular Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), a parameter of the PDF, the mean value.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF, the standard deviation.
!
!    Output, real ( kind = 8 ) VARIANCE(2), the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b
  real ( kind = 8 ) variance(2)

  variance(1) = b ** 2
  variance(2) = b ** 2

  return
end

subroutine circular_normal_mean ( a, b, mean )

!*****************************************************************************80
!
!! CIRCULAR_NORMAL_MEAN returns the mean of the Circular Normal PDF.
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
!    Output, real ( kind = 8 ) MEAN(2), the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a(2)
  real ( kind = 8 ) b
  real ( kind = 8 ) mean(2)

  mean(1:2) = a(1:2)

  return
end

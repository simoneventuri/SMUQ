subroutine chi_square_noncentral_mean ( a, b, mean )

!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_MEAN: mean of the noncentral Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the parameter of the PDF.
!    1.0D+00 <= A.
!
!    Input, real ( kind = 8 ) B, the noncentrality parameter of the PDF.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) MEAN, the mean value.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mean

  mean = a + b

  return
end

subroutine chi_square_noncentral_variance ( a, b, variance )

!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_VARIANCE: variance of the noncentral Chi squared PDF.
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
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the noncentrality parameter of the PDF.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance value.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) variance

  variance = 2.0D+00 * ( a + 2.0D+00 * b )

  return
end

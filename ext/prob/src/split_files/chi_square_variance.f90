subroutine chi_square_variance ( a, variance )

!*****************************************************************************80
!
!! CHI_SQUARE_VARIANCE returns the variance of the central Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the distribution.
!    1 <= A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) variance

  variance = 2.0D+00 * a

  return
end

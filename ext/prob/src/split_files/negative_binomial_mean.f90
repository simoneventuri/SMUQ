subroutine negative_binomial_mean ( a, b, mean )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_MEAN returns the mean of the Negative Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) mean

  mean = real ( a, kind = 8 ) / b

  return
end

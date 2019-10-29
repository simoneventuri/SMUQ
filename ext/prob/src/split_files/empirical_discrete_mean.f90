subroutine empirical_discrete_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_MEAN returns the mean of the Empirical Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) c(a)
  real ( kind = 8 ) mean

  mean = dot_product ( b(1:a), c(1:a) ) / sum ( b(1:a) )

  return
end

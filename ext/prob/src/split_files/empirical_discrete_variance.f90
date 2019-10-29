subroutine empirical_discrete_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_VARIANCE: variance of the Empirical Discrete PDF.
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
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) bsum
  real ( kind = 8 ) c(a)
  integer ( kind = 4 ) i
  real ( kind = 8 ) mean
  real ( kind = 8 ) variance

  bsum = sum ( b(1:a) )

  call empirical_discrete_mean ( a, b, c, mean )

  variance = 0.0D+00

  do i = 1, a
    variance = variance + ( b(i) / bsum ) * ( c(i) - mean ) ** 2
  end do

  return
end

subroutine empirical_discrete_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_CDF evaluates the Empirical Discrete CDF.
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
!    Input, real ( kind = 8 ) X, the argument of the CDF.
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
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) bsum
  real ( kind = 8 ) c(a)
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) i
  real ( kind = 8 ) x

  cdf = 0.0D+00

  bsum = sum ( b(1:a) )

  do i = 1, a

    if ( x < c(i) ) then
      return
    end if

    cdf = cdf + b(i) / bsum

  end do

  return
end

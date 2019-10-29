subroutine empirical_discrete_cdf_inv ( cdf, a, b, c, x )

!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_CDF_INV inverts the Empirical Discrete CDF.
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
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
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
!    Output, real ( kind = 8 ) X, the smallest argument whose CDF is greater
!    than or equal to CDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) bsum
  real ( kind = 8 ) c(a)
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf2
  integer ( kind = 4 ) i
  real ( kind = 8 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  bsum = sum ( b(1:a) )

  x = c(1)
  cdf2 = b(1) / bsum

  do i = 2, a

    if ( cdf <= cdf2 ) then
      return
    end if

    x = c(i)
    cdf2 = cdf2 + b(i) / bsum

  end do

  return
end

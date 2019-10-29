subroutine discrete_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! DISCRETE_CDF_INV inverts the Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
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
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of outcomes
!    1 through A.  Each entry must be nonnegative.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument for which
!    CDF(X-1) < CDF <= CDF(X)
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) b_sum
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cum
  integer ( kind = 4 ) j
  integer ( kind = 4 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DISCRETE_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  b_sum = sum ( b(1:a) )

  cum = 0.0D+00

  do j = 1, a

    cum = cum + b(j) / b_sum

    if ( cdf <= cum ) then
      x = j
      return
    end if

  end do

  x = a

  return
end

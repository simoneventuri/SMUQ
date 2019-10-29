subroutine quasigeometric_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! QUASIGEOMETRIC_CDF_INV inverts the Quasigeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0D+00
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding value of X.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'QUASIGEOMETRIC_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf < a ) then
    x = 0
  else if ( b == 0.0D+00 ) then
    x = 1
  else
    x = 1 + int ( ( log ( 1.0D+00 - cdf ) - log ( 1.0D+00 - a ) ) / log ( b ) )
  end if

  return
end

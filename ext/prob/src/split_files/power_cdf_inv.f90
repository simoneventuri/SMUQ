subroutine power_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! POWER_CDF_INV inverts the Power CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2000
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
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, real ( kind = 8 ) X, the argument of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POWER_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf == 0.0D+00 ) then
    x = 0.0D+00
  else if ( cdf < 1.0D+00 ) then
    x = b * exp ( log ( cdf ) / a )
  else
    x = b
  end if

  return
end

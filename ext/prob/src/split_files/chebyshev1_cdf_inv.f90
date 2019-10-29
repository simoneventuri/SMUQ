subroutine chebyshev1_cdf_inv ( cdf, x )

!*****************************************************************************80
!
!! CHEBYSHEV1_CDF_INV inverts the Chebyshev1 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
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
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) cdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHEBYSHEV1_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  x = sin ( r8_pi * ( cdf - 0.5 ) )

  return
end

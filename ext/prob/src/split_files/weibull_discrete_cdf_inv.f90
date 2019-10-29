subroutine weibull_discrete_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! WEIBULL_DISCRETE_CDF_INV inverts the Discrete Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2004
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
!    0.0 <= A <= 1.0D+00,
!    0.0 < B.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) r8_ceiling
  integer ( kind = 4 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WEIBULL_DISCRETE_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  x = r8_ceiling ( ( log ( 1.0D+00 - cdf ) / log ( 1.0D+00 - a ) ) &
    ** ( 1.0D+00 / b ) - 1.0D+00 )

  return
end

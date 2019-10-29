subroutine frechet_cdf_inv ( cdf, alpha, x )

!*****************************************************************************80
!
!! FRECHET_CDF_INV inverts the Frechet CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
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
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 0.0 < ALPHA.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FRECHET_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FRECHET_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  ALPHA <= 0.0.'
    stop 1
  end if

  if ( cdf == 0.0D+00 ) then
    x = 0.0D+00
  else
    x =  ( - 1.0D+00 / log ( cdf ) ) ** ( 1.0D+00 / alpha )
  end if

  return
end

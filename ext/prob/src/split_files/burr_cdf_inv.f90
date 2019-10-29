subroutine burr_cdf_inv ( cdf, a, b, c, d, x )

!*****************************************************************************80
!
!! BURR_CDF_INV inverts the Burr CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
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
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) d
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BURR_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  y = ( ( 1.0D+00 / ( 1.0D+00 - cdf ) ) ** ( 1.0D+00 / d ) &
    - 1.0D+00 ) ** ( 1.0D+00 / c )

  x = a + b * y

  return
end

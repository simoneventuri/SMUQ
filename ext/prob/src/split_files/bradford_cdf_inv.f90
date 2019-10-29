subroutine bradford_cdf_inv ( cdf, a, b, c, x )

!*****************************************************************************80
!
!! BRADFORD_CDF_INV inverts the Bradford CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
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
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BRADFORD_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf <= 0.0D+00 ) then
    x = a
  else if ( cdf < 1.0D+00 ) then
    x = a + ( b - a ) * ( ( c + 1.0D+00 ) ** cdf - 1.0D+00 ) / c
  else if ( 1.0D+00 <= cdf ) then
    x = b
  end if

  return
end

subroutine triangle_cdf_inv ( cdf, a, b, c, x )

!*****************************************************************************80
!
!! TRIANGLE_CDF_INV inverts the Triangle CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2004
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
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf_mid
  real ( kind = 8 ) d
  real ( kind = 8 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIANGLE_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  d = 2.0D+00 / ( c - a )
  cdf_mid = 0.5D+00 * d * ( b - a )

  if ( cdf <= cdf_mid ) then
    x = a + sqrt ( cdf * ( b - a ) * ( c - a ) )
  else
    x = c - sqrt ( ( c - b ) * ( ( c - b ) - ( cdf - cdf_mid ) * ( c - a ) ) )
  end if

  return
end

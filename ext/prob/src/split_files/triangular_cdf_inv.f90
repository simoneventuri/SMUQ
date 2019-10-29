subroutine triangular_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! TRIANGULAR_CDF_INV inverts the Triangular CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
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
!    A < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIANGULAR_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf <= 0.5D+00 ) then
    x = a + 0.5D+00 * ( b - a ) * sqrt ( 2.0D+00 * cdf )
  else
    x = b - 0.5D+00 * ( b - a ) * sqrt ( 2.0D+00 * ( 1.0D+00 - cdf ) )
  end if

  return
end

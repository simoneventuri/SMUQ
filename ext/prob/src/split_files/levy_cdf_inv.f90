subroutine levy_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! LEVY_CDF_INV inverts the Levy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
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
!    0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf1
  real ( kind = 8 ) x
  real ( kind = 8 ) x1

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LEVY_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LEVY_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  Input parameter B <= 0.0'
    stop 1
  end if

  cdf1 = 1.0D+00 - 0.5D+00 * cdf
  call normal_01_cdf_inv ( cdf1, x1 )
  x = a + b / ( x1 * x1 )

  return
end

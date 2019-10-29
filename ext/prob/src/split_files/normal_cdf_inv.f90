subroutine normal_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! NORMAL_CDF_INV inverts the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
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
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NORMAL_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  call normal_01_cdf_inv ( cdf, x2 )

  x = a + b * x2

  return
end

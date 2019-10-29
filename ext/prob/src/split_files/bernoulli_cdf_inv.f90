subroutine bernoulli_cdf_inv ( cdf, a, x )

!*****************************************************************************80
!
!! BERNOULLI_CDF_INV inverts the Bernoulli CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 1999
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
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 <= A <= 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BERNOULLI_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf <= 1.0D+00 - a ) then
    x = 0
  else
    x = 1
  end if

  return
end

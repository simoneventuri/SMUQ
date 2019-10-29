subroutine geometric_cdf_inv ( cdf, a, x )

!*****************************************************************************80
!
!! GEOMETRIC_CDF_INV inverts the Geometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0D+00
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding value of X.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GEOMETRIC_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( a == 1.0D+00 ) then
    x = 1
  else if ( a == 0.0D+00 ) then
    x = huge ( x )
  else
    x = 1 + int ( log ( 1.0D+00 - cdf ) / log ( 1.0D+00 - a ) )
  end if

  return
end

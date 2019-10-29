subroutine uniform_discrete_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! UNIFORM_DISCRETE_CDF_INV inverts the Uniform Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
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
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, integer ( kind = 4 ) X, the smallest argument whose CDF is greater
!    than or equal to CDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) a2
  integer ( kind = 4 ) b
  real ( kind = 8 ) b2
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) x
  real ( kind = 8 ) x2

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'UNIFORM_DISCRETE_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  a2 = real ( a, kind = 8 ) - 0.5D+00
  b2 = real ( b, kind = 8 ) + 0.5D+00
  x2 = a + cdf * ( b2 - a2 )

  x = nint ( x2 )

  x = max ( x, a )
  x = min ( x, b )

  return
end

subroutine cosine_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! COSINE_CDF_INV inverts the Cosine CDF.
!
!  Discussion:
!
!    A simple bisection method is used on the interval
!    [ A - PI * B, A + PI * B ].
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
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf1
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) cdf3
  integer ( kind = 4 ) it
  integer ( kind = 4 ), parameter :: it_max = 100
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ), parameter :: tol = 0.0001D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COSINE_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf == 0.0D+00 ) then
    x = a - r8_pi * b
    return
  else if ( 1.0D+00 == cdf ) then
    x = a + r8_pi * b
    return
  end if

  x1 = a - r8_pi * b
  cdf1 = 0.0D+00

  x2 = a + r8_pi * b
  cdf2 = 1.0D+00
!
!  Now use bisection.
!
  it = 0

  do it = 1, it_max

    x3 = 0.5D+00 * ( x1 + x2 )
    call cosine_cdf ( x3, a, b, cdf3 )

    if ( abs ( cdf3 - cdf ) < tol ) then
      x = x3
      return
    end if

    if ( sign ( 1.0D+00, cdf3 - cdf ) == sign ( 1.0D+00, cdf1 - cdf ) ) then
      x1 = x3
      cdf1 = cdf3
    else
      x2 = x3
      cdf2 = cdf3
    end if

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COSINE_CDF_INV - Fatal error!'
  write ( *, '(a)' ) '  Iteration limit exceeded.'

  stop 1
end

subroutine chi_cdf_inv ( cdf, a, b, c, x )

!*****************************************************************************80
!
!! CHI_CDF_INV inverts the Chi CDF.
!
!  Discussion:
!
!    A simple bisection method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf1
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) cdf3
  integer ( kind = 4 ) it
  integer ( kind = 4 ), parameter :: it_max = 100
  real ( kind = 8 ), parameter :: tol = 0.0001D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf == 0.0D+00 ) then
    x = a
    return
  else if ( 1.0D+00 == cdf ) then
    x = huge ( x )
    return
  end if

  x1 = a
  cdf1 = 0.0D+00

  x2 = a + 1.0D+00

  do

    call chi_cdf ( x2, a, b, c, cdf2 )

    if ( cdf < cdf2 ) then
      exit
    end if

    x2 = a + 2.0D+00 * ( x2 - a )

  end do
!
!  Now use bisection.
!
  it = 0

  do

    it = it + 1

    x3 = 0.5D+00 * ( x1 + x2 )
    call chi_cdf ( x3, a, b, c, cdf3 )

    if ( abs ( cdf3 - cdf ) < tol ) then
      x = x3
      return
    end if

    if ( it_max < it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CHI_CDF_INV - Fatal error!'
      write ( *, '(a)' ) '  Iteration limit exceeded.'
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

  return
end

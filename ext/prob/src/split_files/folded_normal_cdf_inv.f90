subroutine folded_normal_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! FOLDED_NORMAL_CDF_INV inverts the Folded Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2001
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
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the argument of the CDF.
!    0.0 <= X.
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
  real ( kind = 8 ), parameter :: tol = 0.0001D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3
  real ( kind = 8 ) xa
  real ( kind = 8 ) xb

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FOLDED_NORMAL_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf == 0.0D+00 ) then
    x = 0.0D+00
    return
  else if ( 1.0D+00 == cdf ) then
    x = huge ( x )
    return
  end if
!
!  Find X1, for which the value of CDF will be too small.
!
  if ( 0.0D+00 <= a ) then
    call normal_cdf_inv ( cdf, a, b, x1 )
  else
    call normal_cdf_inv ( cdf, -a, b, x1 )
  end if
  x1 = max ( x1, 0.0D+00 )
  call folded_normal_cdf ( x1, a, b, cdf1 )
!
!  Find X2, for which the value of CDF will be too big.
!
  cdf2 = ( 1.0D+00 - cdf ) / 2.0D+00

  call normal_cdf_inv ( cdf2, a, b, xa )
  call normal_cdf_inv ( cdf2, -a, b, xb )
  x2 = max ( abs ( xa ), abs ( xb ) )
  call folded_normal_cdf ( x2, a, b, cdf2 )
!
!  Now use bisection.
!
  it = 0

  do

    it = it + 1

    x3 = 0.5D+00 * ( x1 + x2 )
    call folded_normal_cdf ( x3, a, b, cdf3 )

    if ( abs ( cdf3 - cdf ) < tol ) then
      x = x3
      exit
    end if

    if ( it_max < it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FOLDED_NORMAL_CDF_INV - Fatal error!'
      write ( *, '(a)' ) '  Iteration limit exceeded.'
      stop 1
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

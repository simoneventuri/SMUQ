subroutine von_mises_cdf_inv ( cdf, a, b, x )

!*****************************************************************************80
!
!! VON_MISES_CDF_INV inverts the von Mises CDF.
!
!  Discussion:
!
!    A simple bisection method is used on the interval [ A - PI, A + PI ].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!    A - PI <= X <= A + PI.
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
  real ( kind = 8 ), parameter :: tol = 0.000001D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) x1
  real ( kind = 8 ) x2
  real ( kind = 8 ) x3

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'VON_MISES_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  if ( cdf == 0.0D+00 ) then
    x = a - r8_pi
    return
  else if ( 1.0D+00 == cdf ) then
    x = a + r8_pi
    return
  end if

  x1 = a - r8_pi
  cdf1 = 0.0D+00

  x2 = a + r8_pi
  cdf2 = 1.0D+00
!
!  Now use bisection.
!
  it = 0

  do

    it = it + 1

    x3 = 0.5D+00 * ( x1 + x2 )
    call von_mises_cdf ( x3, a, b, cdf3 )

    if ( abs ( cdf3 - cdf ) < tol ) then
      x = x3
      exit
    end if

    if ( it_max < it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'VON_MISES_CDF_INV - Fatal error!'
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

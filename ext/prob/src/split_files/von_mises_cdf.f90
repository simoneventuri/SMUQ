subroutine von_mises_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! VON_MISES_CDF evaluates the von Mises CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2005
!
!  Author:
!
!    Original FORTRAN77 version by Geoffrey Hill.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Geoffrey Hill,
!    Algorithm 518,
!    Incomplete Bessel Function I0: The von Mises Distribution,
!    ACM Transactions on Mathematical Software,
!    Volume 3, Number 3, September 1977, pages 279-284.
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    QA276.M335
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    A - PI <= X <= A + PI.
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
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: a1 = 12.0D+00
  real ( kind = 8 ), parameter :: a2 = 0.8D+00
  real ( kind = 8 ), parameter :: a3 = 8.0D+00
  real ( kind = 8 ), parameter :: a4 = 1.0D+00
  real ( kind = 8 ) arg
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: c1 = 56.0D+00
  real ( kind = 8 ) cdf
  real ( kind = 8 ), parameter :: ck = 10.5D+00
  real ( kind = 8 ) cn
  real ( kind = 8 ) erfx
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) n
  real ( kind = 8 ) p
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_error_f
  real ( kind = 8 ) s
  real ( kind = 8 ) sn
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z
!
!  We expect -PI <= X - A <= PI.
!
  if ( x - a <= - r8_pi ) then
    cdf = 0.0D+00
    return
  end if

  if ( r8_pi <= x - a ) then
    cdf = 1.0D+00
    return
  end if
!
!  Convert the angle (X - A) modulo 2 PI to the range ( 0, 2 * PI ).
!
  z = b

  u = mod ( x - a + r8_pi, 2.0D+00 * r8_pi )

  if ( u < 0.0D+00 ) then
    u = u + 2.0D+00 * r8_pi
  end if

  y = u - r8_pi
!
!  For small B, sum IP terms by backwards recursion.
!
  if ( z <= ck ) then

    v = 0.0D+00

    if ( 0.0D+00 < z ) then

      ip = int ( z * a2 - a3 / ( z + a4 ) + a1 )
      p = real ( ip, kind = 8 )
      s = sin ( y )
      c = cos ( y )
      y = p * y
      sn = sin ( y )
      cn = cos ( y )
      r = 0.0D+00
      z = 2.0D+00 / z

      do n = 2, ip
        p = p - 1.0D+00
        y = sn
        sn = sn * c - cn * s
        cn = cn * c + y * s
        r = 1.0D+00 / ( p * z + r )
        v = ( sn / p + v ) * r
      end do

    end if

    cdf = ( u * 0.5D+00 + v ) / r8_pi
!
!  For large B, compute the normal approximation and left tail.
!
  else

    c = 24.0D+00 * z
    v = c - c1
    r = sqrt ( ( 54.0D+00 / ( 347.0D+00 / v + 26.0D+00 - c ) - 6.0D+00 + c ) &
      / 12.0D+00 )
    z = sin ( 0.5D+00 * y ) * r
    s = 2.0D+00 * z ** 2
    v = v - s + 3.0D+00
    y = ( c - s - s - 16.0D+00 ) / 3.0D+00
    y = ( ( s + 1.75D+00 ) * s + 83.5D+00 ) / v - y
    arg = z * ( 1.0D+00 - s / y ** 2 )
    erfx = r8_error_f ( arg )
    cdf = 0.5D+00 * erfx + 0.5D+00

  end if

  cdf = max ( cdf, 0.0D+00 )
  cdf = min ( cdf, 1.0D+00 )

  return
end

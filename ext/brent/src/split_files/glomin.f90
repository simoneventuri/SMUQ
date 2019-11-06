function glomin ( a, b, c, m, machep, e, t, f, x )

!*****************************************************************************80
!
!! GLOMIN seeks a global minimum of a function F(X) in an interval [A,B].
!
!  Discussion:
!
!    This function assumes that F(X) is twice continuously differentiable
!    over [A,B] and that F''(X) <= M for all X in [A,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2008
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization Without Derivatives,
!    Dover, 2002,
!    ISBN: 0-486-41998-3,
!    LC: QA402.5.B74.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the interval.
!    It must be the case that A < B.
!
!    Input, real ( kind = 8 ) C, an initial guess for the global
!    minimizer.  If no good guess is known, C = A or B is acceptable.
!
!    Input, real ( kind = 8 ) M, the bound on the second derivative.
!
!    Input, real ( kind = 8 ) MACHEP, an estimate for the relative machine
!    precision.
!
!    Input, real ( kind = 8 ) E, a positive tolerance, a bound for the
!    absolute error in the evaluation of F(X) for any X in [A,B].
!
!    Input, real ( kind = 8 ) T, a positive error tolerance.
!
!    Input, external real ( kind = 8 ) F, the name of a user-supplied
!    function, of the form "FUNCTION F ( X )", which evaluates the
!    function whose global minimum is being sought.
!
!    Output, real ( kind = 8 ) X, the estimated value of the abscissa
!    for which F attains its global minimum value in [A,B].
!
!    Output, real ( kind = 8 ) GLOMIN, the value F(X).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) a2
  real ( kind = 8 ) a3
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d0
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  real ( kind = 8 ) e
  real ( kind = 8 ) f
  real ( kind = 8 ) glomin
  real ( kind = 8 ) h
  integer ( kind = 4 ) k
  real ( kind = 8 ) m
  real ( kind = 8 ) m2
  real ( kind = 8 ) machep
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) qs
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) sc
  real ( kind = 8 ) t
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) yb
  real ( kind = 8 ) z0
  real ( kind = 8 ) z1
  real ( kind = 8 ) z2

  a0 = b
  x = a0
  a2 = a
  y0 = f ( b )
  yb = y0
  y2 = f ( a )
  y = y2

  if ( y0 < y ) then
    y = y0
  else
    x = a
  end if

  if ( m <= 0.0D+00 .or. b <= a ) then
    glomin = y
    return
  end if

  m2 = 0.5D+00 * ( 1.0D+00 + 16.0D+00 * machep ) * m

  if ( c <= a .or. b <= c ) then
    sc = 0.5D+00 * ( a + b )
  else
    sc = c
  end if

  y1 = f ( sc )
  k = 3
  d0 = a2 - sc
  h = 9.0D+00 / 11.0D+00

  if ( y1 < y ) then
    x = sc
    y = y1
  end if

  do

    d1 = a2 - a0
    d2 = sc - a0
    z2 = b - a2
    z0 = y2 - y1
    z1 = y2 - y0
    r = d1 * d1 * z0 - d0 * d0 * z1
    p = r
    qs = 2.0D+00 * ( d0 * z1 - d1 * z0 )
    q = qs

    if ( k < 1000000 .or. y2 <= y ) then

      do

        if ( q * ( r * ( yb - y2 ) + z2 * q * ( ( y2 - y ) + t ) ) < &
          z2 * m2 * r * ( z2 * q - r ) ) then
          a3 = a2 + r / q
          y3 = f ( a3 )

          if ( y3 < y ) then
            x = a3
            y = y3
          end if
        end if

        k = mod ( 1611 * k, 1048576 )
        q = 1.0D+00
        r = ( b - a ) * 0.00001D+00 * real ( k, kind = 8 )

        if ( z2 <= r ) then
          exit
        end if

      end do

    else

      k = mod ( 1611 * k, 1048576 )
      q = 1.0D+00
      r = ( b - a ) * 0.00001D+00 * real ( k, kind = 8 )

      do while ( r < z2 )

        if ( q * ( r * ( yb - y2 ) + z2 * q * ( ( y2 - y ) + t ) ) < &
          z2 * m2 * r * ( z2 * q - r ) ) then
          a3 = a2 + r / q
          y3 = f ( a3 )

          if ( y3 < y ) then
            x = a3
            y = y3
          end if
        end if

        k = mod ( 1611 * k, 1048576 )
        q = 1.0D+00
        r = ( b - a ) * 0.00001D+00 * real ( k, kind = 8 )

      end do

    end if

    r = m2 * d0 * d1 * d2
    s = sqrt ( ( ( y2 - y ) + t ) / m2 )
    h = 0.5D+00 * ( 1.0D+00 + h )
    p = h * ( p + 2.0D+00 * r * s )
    q = q + 0.5D+00 * qs
    r = - 0.5D+00 * ( d0 + ( z0 + 2.01D+00 * e ) / ( d0 * m2 ) )

    if ( r < s .or. d0 < 0.0D+00 ) then
      r = a2 + s
    else
      r = a2 + r
    end if

    if ( 0.0D+00 < p * q ) then
      a3 = a2 + p / q
    else
      a3 = r
    end if

    do

      a3 = max ( a3, r )

      if ( b <= a3 ) then
        a3 = b
        y3 = yb
      else
        y3 = f ( a3 )
      end if

      if ( y3 < y ) then
        x = a3
        y = y3
      end if

      d0 = a3 - a2

      if ( a3 <= r ) then
        exit
      end if

      p = 2.0D+00 * ( y2 - y3 ) / ( m * d0 )

      if ( ( 1.0D+00 + 9.0D+00 * machep ) * d0 <= abs ( p ) ) then
        exit
      end if

      if ( 0.5D+00 * m2 * ( d0 * d0 + p * p ) <= &
        ( y2 - y ) + ( y3 - y ) + 2.0D+00 * t ) then
        exit
      end if

      a3 = 0.5D+00 * ( a2 + a3 )
      h = 0.9D+00 * h

    end do

    if ( b <= a3 ) then
      exit
    end if

    a0 = sc
    sc = a2
    a2 = a3
    y0 = y1
    y1 = y2
    y2 = y3

  end do

  glomin = y

  return
end

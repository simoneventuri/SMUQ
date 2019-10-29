subroutine gamma_inc_inv ( a, x, x0, p, q, ierr )

!*****************************************************************************80
!
!! GAMMA_INC_INV computes the inverse incomplete gamma ratio function.
!
!  Discussion:
!
!    The routine is given positive A, and nonnegative P and Q where P + Q = 1.
!    The value X is computed with the property that P(A,X) = P and Q(A,X) = Q.
!    Schroder iteration is employed.  The routine attempts to compute X
!    to 10 significant digits if this is possible for the particular computer
!    arithmetic being used.
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter in the incomplete gamma
!    ratio.  A must be positive.
!
!    Output, real ( kind = 8 ) X, the computed point for which the
!    incomplete gamma functions have the values P and Q.
!
!    Input, real ( kind = 8 ) X0, an optional initial approximation
!    for the solution X.  If the user does not want to supply an
!    initial approximation, then X0 should be set to 0, or a negative
!    value.
!
!    Input, real ( kind = 8 ) P, Q, the values of the incomplete gamma
!    functions, for which the corresponding argument is desired.
!
!    Output, integer ( kind = 4 ) IERR, error flag.
!    0, the solution was obtained. Iteration was not used.
!    0 < K, The solution was obtained. IERR iterations were performed.
!    -2, A <= 0
!    -3, No solution was obtained. The ratio Q/A is too large.
!    -4, P + Q /= 1
!    -6, 20 iterations were performed. The most recent value obtained
!        for X is given.  This cannot occur if X0 <= 0.
!    -7, Iteration failed. No value is given for X.
!        This may occur when X is approximately 0.
!    -8, A value for X has been obtained, but the routine is not certain
!        of its accuracy.  Iteration cannot be performed in this
!        case. If X0 <= 0, this can occur only when P or Q is
!        approximately 0. If X0 is positive then this can occur when A is
!        exceedingly close to X and A is extremely large (say A .GE. 1.E20).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: a0 = 3.31125922108741D+00
  real ( kind = 8 ), parameter :: a1 = 11.6616720288968D+00
  real ( kind = 8 ), parameter :: a2 = 4.28342155967104D+00
  real ( kind = 8 ), parameter :: a3 = 0.213623493715853D+00
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) am1
  real ( kind = 8 ) amax
  real ( kind = 8 ), dimension(2) :: amin = (/ &
    500.0D+00, 100.0D+00 /)
  real ( kind = 8 ) ap1
  real ( kind = 8 ) ap2
  real ( kind = 8 ) ap3
  real ( kind = 8 ) apn
  real ( kind = 8 ) b
  real ( kind = 8 ), parameter :: b1 = 6.61053765625462D+00
  real ( kind = 8 ), parameter :: b2 = 6.40691597760039D+00
  real ( kind = 8 ), parameter :: b3 = 1.27364489782223D+00
  real ( kind = 8 ), parameter :: b4 = .036117081018842D+00
  real ( kind = 8 ), dimension ( 2 ) :: bmin = (/ &
    1.0D-28, 1.0D-13 /)
  real ( kind = 8 ), parameter :: c = 0.577215664901533D+00
  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) c3
  real ( kind = 8 ) c4
  real ( kind = 8 ) c5
  real ( kind = 8 ) d
  real ( kind = 8 ), dimension ( 2 ) :: dmin = (/ &
    1.0D-06, 1.0D-04 /)
  real ( kind = 8 ) e
  real ( kind = 8 ) e2
  real ( kind = 8 ), dimension ( 2 ) :: emin = (/ &
    2.0D-03, 6.0D-03 /)
  real ( kind = 8 ) eps
  real ( kind = 8 ), dimension ( 2 ) :: eps0 = (/ &
    1.0D-10, 1.0D-08 /)
  real ( kind = 8 ) g
  real ( kind = 8 ) gamma_log
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ) gamma
  real ( kind = 8 ) h
  real ( kind = 8 ), parameter :: half = 0.5D+00
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) iop
  real ( kind = 8 ), parameter :: ln10 = 2.302585D+00
  real ( kind = 8 ) p
  real ( kind = 8 ) pn
  real ( kind = 8 ) q
  real ( kind = 8 ) qg
  real ( kind = 8 ) qn
  real ( kind = 8 ) r
  real ( kind = 8 ) rcomp
  real ( kind = 8 ) rta
  real ( kind = 8 ) s
  real ( kind = 8 ) s2
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ), parameter :: tol = 1.0D-05
  real ( kind = 8 ), parameter :: two =  2.0D+00
  real ( kind = 8 ) u
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) x0
  real ( kind = 8 ) xn
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  e = epsilon ( e )

  x = 0.0D+00

  if ( a <= 0.0D+00 ) then
    ierr = -2
    return
  end if

  t = p + q - 1.0D+00

  if ( e < abs ( t ) ) then
    ierr = -4
    return
  end if

  ierr = 0

  if ( p == 0.0D+00 ) then
    return
  end if

  if ( q == 0.0D+00 ) then
    x = huge ( x )
    return
  end if

  if ( a == 1.0D+00 ) then
    if ( 0.9D+00 <= q ) then
      x = -alnrel ( - p )
    else
      x = -log ( q )
    end if
    return
  end if

  e2 = two * e
  amax = 0.4D-10 / ( e * e )

  if ( 1.0D-10 < e ) then
    iop = 2
  else
    iop = 1
  end if

  eps = eps0(iop)
  xn = x0

  if ( 0.0D+00 < x0 ) then
    go to 160
  end if
!
!  Selection of the initial approximation XN of X when A < 1.
!
  if ( 1.0D+00 < a ) then
    go to 80
  end if

  g = gamma ( a + 1.0D+00 )
  qg = q * g

  if ( qg == 0.0D+00 ) then
    x = huge ( x )
    ierr = -8
    return
  end if

  b = qg / a

  if ( 0.6D+00 * a < qg ) then
    go to 40
  end if

  if ( a < 0.30D+00 .and. 0.35D+00 <= b ) then
    t = exp ( - ( b + c ) )
    u = t * exp ( t )
    xn = t * exp ( u )
    go to 160
  end if

  if ( 0.45D+00 <= b ) then
    go to 40
  end if

  if ( b == 0.0D+00 ) then
    x = huge ( x )
    ierr = -8
    return
  end if

  y = -log ( b )
  s = half + ( half - a )
  z = log ( y )
  t = y - s * z

  if ( 0.15D+00 <= b ) then
    xn = y - s * log ( t ) - log ( 1.0D+00 + s / ( t + 1.0D+00 ) )
    go to 220
  end if

  if ( 0.01D+00 < b ) then
    u = ( ( t + two * ( 3.0D+00 - a ) ) * t &
      + ( two - a ) * ( 3.0D+00 - a )) / &
      ( ( t + ( 5.0D+00 - a ) ) * t + two )
    xn = y - s * log ( t ) - log ( u )
    go to 220
  end if

30 continue

  c1 = -s * z
  c2 = -s * ( 1.0D+00 + c1 )

  c3 = s * (( half * c1 &
    + ( two - a ) ) * c1 + ( 2.5D+00 - 1.5D+00 * a ) )

  c4 = -s * ((( c1 / 3.0D+00 + ( 2.5D+00 - 1.5D+00 * a ) ) * c1 &
    + ( ( a - 6.0D+00 ) * a + 7.0D+00 ) ) &
    * c1 + ( ( 11.0D+00 * a - 46.0D+00 ) * a + 47.0D+00 ) / 6.0D+00 )

  c5 = -s * (((( - c1 / 4.0D+00 + ( 11.0D+00 * a - 17.0D+00 ) / 6.0D+00 ) * c1 &
     + ( ( -3.0D+00 * a + 13.0D+00 ) * a - 13.0D+00 ) ) * c1 &
     + half &
     * ( ( ( two * a - 25.0D+00 ) * a + 72.0D+00 ) &
     * a - 61.0D+00 ) ) * c1 &
     + ( ( ( 25.0D+00 * a - 195.0D+00 ) * a &
     + 477.0D+00 ) * a - 379.0D+00 ) / 12.0D+00 )

  xn = (((( c5 / y + c4 ) / y + c3 ) / y + c2 ) / y + c1 ) + y

  if ( 1.0D+00 < a ) then
    go to 220
  end if

  if ( bmin(iop) < b ) then
    go to 220
  end if

  x = xn
  return

   40 continue

  if ( b * q <= 1.0D-08 ) then
    xn = exp ( - ( q / a + c ))
  else if ( 0.9D+00 < p ) then
    xn = exp ( ( alnrel ( - q ) + gamma_ln1 ( a )) / a )
  else
    xn = exp ( log ( p * g ) / a )
  end if

  if ( xn == 0.0D+00 ) then
    ierr = -3
    return
  end if

  t = half + ( half - xn / ( a + 1.0D+00 ))
  xn = xn / t
  go to 160
!
!  Selection of the initial approximation XN of X when 1 < A.
!
   80 continue

  if ( 0.5D+00 < q ) then
    w = log ( p )
  else
    w = log ( q )
  end if

  t = sqrt ( - two * w )

  s = t - ((( a3 * t + a2 ) * t + a1 ) * t + a0 ) / (((( &
    b4 * t + b3 ) * t + b2 ) * t + b1 ) * t + 1.0D+00 )

  if ( 0.5D+00 < q ) then
    s = -s
  end if

  rta = sqrt ( a )
  s2 = s * s

  xn = a + s * rta + ( s2 - 1.0D+00 ) / 3.0D+00 + s * ( s2 - 7.0D+00 ) &
    / ( 36.0D+00 * rta ) - ( ( 3.0D+00 * s2 + 7.0D+00 ) * s2 - 16.0D+00 ) &
    / ( 810.0D+00 * a ) + s * (( 9.0D+00 * s2 + 256.0D+00 ) * s2 - 433.0D+00 ) &
    / ( 38880.0D+00 * a * rta )

  xn = max ( xn, 0.0D+00 )

  if ( amin(iop) <= a ) then

    x = xn
    d = half + ( half - x / a )

    if ( abs ( d ) <= dmin(iop) ) then
      return
    end if

  end if

  110 continue

  if ( p <= 0.5D+00 ) then
    go to 130
  end if

  if ( xn < 3.0D+00 * a ) then
    go to 220
  end if

  y = - ( w + gamma_log ( a ) )
  d = max ( two, a * ( a - 1.0D+00 ) )

  if ( ln10 * d <= y ) then
    s = 1.0D+00 - a
    z = log ( y )
    go to 30
  end if

  120 continue

  t = a - 1.0D+00
  xn = y + t * log ( xn ) - alnrel ( -t / ( xn + 1.0D+00 ) )
  xn = y + t * log ( xn ) - alnrel ( -t / ( xn + 1.0D+00 ) )
  go to 220

  130 continue

  ap1 = a + 1.0D+00

  if ( 0.70D+00 * ap1 < xn ) then
    go to 170
  end if

  w = w + gamma_log ( ap1 )

  if ( xn <= 0.15 * ap1 ) then
    ap2 = a + two
    ap3 = a + 3.0D+00
    x = exp ( ( w + x ) / a )
    x = exp ( ( w + x - log ( 1.0D+00 + ( x / ap1 ) &
      * ( 1.0D+00 + x / ap2 ) ) ) / a )
    x = exp ( ( w + x - log ( 1.0D+00 + ( x / ap1 ) &
      * ( 1.0D+00 + x / ap2 ) ) ) / a )
    x = exp ( ( w + x - log ( 1.0D+00 + ( x / ap1 ) &
      * ( 1.0D+00 + ( x / ap2 ) &
      * ( 1.0D+00 + x / ap3 ) ) ) ) / a )
    xn = x

    if ( xn <= 1.0D-02 * ap1 ) then
      if ( xn <= emin(iop) * ap1 ) then
        return
      end if
      go to 170
    end if

  end if

  apn = ap1
  t = xn / apn
  sum1 = 1.0D+00 + t

  do

    apn = apn + 1.0D+00
    t = t * ( xn / apn )
    sum1 = sum1 + t

    if ( t <= 1.0D-04 ) then
      exit
    end if

  end do

  t = w - log ( sum1 )
  xn = exp ( ( xn + t ) / a )
  xn = xn * ( 1.0D+00 - ( a * log ( xn ) - xn - t ) / ( a - xn ) )
  go to 170
!
!  Schroder iteration using P.
!
  160 continue

  if ( 0.5D+00 < p ) then
    go to 220
  end if

  170 continue

  if ( p <= 1.0D+10 * tiny ( p ) ) then
    x = xn
    ierr = -8
    return
  end if

  am1 = ( a - half ) - half

  180 continue

  if ( amax < a ) then
    d = half + ( half - xn / a )
    if ( abs ( d ) <= e2 ) then
      x = xn
      ierr = -8
      return
    end if
  end if

  190 continue

  if ( 20 <= ierr ) then
    ierr = -6
    return
  end if

  ierr = ierr + 1
  call gamma_inc ( a, xn, pn, qn, 0 )

  if ( pn == 0.0D+00 .or. qn == 0.0D+00 ) then
    x = xn
    ierr = -8
    return
  end if

  r = rcomp ( a, xn )

  if ( r == 0.0D+00 ) then
    x = xn
    ierr = -8
    return
  end if

  t = ( pn - p ) / r
  w = half * ( am1 - xn )

  if ( abs ( t ) <= 0.1D+00 .and. abs ( w * t ) <= 0.1D+00 ) then
    go to 200
  end if

  x = xn * ( 1.0D+00 - t )

  if ( x <= 0.0D+00 ) then
    ierr = -7
    return
  end if

  d = abs ( t )
  go to 210

  200 continue

  h = t * ( 1.0D+00 + w * t )
  x = xn * ( 1.0D+00 - h )

  if ( x <= 0.0D+00 ) then
    ierr = -7
    return
  end if

  if ( 1.0D+00 <= abs ( w ) .and. abs ( w ) * t * t <= eps ) then
    return
  end if

  d = abs ( h )

  210 continue

  xn = x

  if ( d <= tol ) then

    if ( d <= eps ) then
      return
    end if

    if ( abs ( p - pn ) <= tol * p ) then
      return
    end if

  end if

  go to 180
!
!  Schroder iteration using Q.
!
  220 continue

  if ( q <= 1.0D+10 * tiny ( q ) ) then
    x = xn
    ierr = -8
    return
  end if

  am1 = ( a - half ) - half

  230 continue

  if ( amax < a ) then
    d = half + ( half - xn / a )
    if ( abs ( d ) <= e2 ) then
      x = xn
      ierr = -8
      return
    end if
  end if

  if ( 20 <= ierr ) then
    ierr = -6
    return
  end if

  ierr = ierr + 1
  call gamma_inc ( a, xn, pn, qn, 0 )

  if ( pn == 0.0D+00 .or. qn == 0.0D+00 ) then
    x = xn
    ierr = -8
    return
  end if

  r = rcomp ( a, xn )

  if ( r == 0.0D+00 ) then
    x = xn
    ierr = -8
    return
  end if

  t = ( q - qn ) / r
  w = half * ( am1 - xn )

  if ( abs ( t ) <= 0.1 .and. abs ( w * t ) <= 0.1 ) then
    go to 250
  end if

  x = xn * ( 1.0D+00 - t )

  if ( x <= 0.0D+00 ) then
    ierr = -7
    return
  end if

  d = abs ( t )
  go to 260

  250 continue

  h = t * ( 1.0D+00 + w * t )
  x = xn * ( 1.0D+00 - h )

  if ( x <= 0.0D+00 ) then
    ierr = -7
    return
  end if

  if ( 1.0D+00 <= abs ( w ) .and. abs ( w ) * t * t <= eps ) then
    return
  end if

  d = abs ( h )

  260 continue

  xn = x

  if ( tol < d ) then
    go to 230
  end if

  if ( d <= eps ) then
    return
  end if

  if ( abs ( q - qn ) <= tol * q ) then
    return
  end if

  go to 230
end

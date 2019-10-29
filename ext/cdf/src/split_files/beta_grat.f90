subroutine beta_grat ( a, b, x, y, w, eps, ierr )

!*****************************************************************************80
!
!! BETA_GRAT evaluates an asymptotic expansion for IX(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.  It is assumed that 15 <= A
!    and B <= 1, and that B is less than A.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Input/output, real ( kind = 8 ) W, a quantity to which the
!    result of the computation is to be added on output.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, integer ( kind = 4 ) IERR, an error flag, which is 0 if no error
!    was detected.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) b
  real ( kind = 8 ) bm1
  real ( kind = 8 ) bp2n
  real ( kind = 8 ) c(30)
  real ( kind = 8 ) cn
  real ( kind = 8 ) coef
  real ( kind = 8 ) d(30)
  real ( kind = 8 ) dj
  real ( kind = 8 ) eps
  real ( kind = 8 ) gam1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierr
  real ( kind = 8 ) j
  real ( kind = 8 ) l
  real ( kind = 8 ) lnx
  integer ( kind = 4 ) n
  real ( kind = 8 ) n2
  real ( kind = 8 ) nu
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) t2
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) z

  bm1 = ( b - 0.5D+00 ) - 0.5D+00
  nu = a + 0.5D+00 * bm1

  if ( y <= 0.375D+00 ) then
    lnx = alnrel ( - y )
  else
    lnx = log ( x )
  end if

  z = -nu * lnx

  if ( b * z == 0.0D+00 ) then
    ierr = 1
    return
  end if
!
!  Computation of the expansion.
!
!  Set R = EXP(-Z)*Z^B/GAMMA(B)
!
  r = b * ( 1.0D+00 + gam1 ( b ) ) * exp ( b * log ( z ))
  r = r * exp ( a * lnx ) * exp ( 0.5D+00 * bm1 * lnx )
  u = algdiv ( b, a ) + b * log ( nu )
  u = r * exp ( - u )

  if ( u == 0.0D+00 ) then
    ierr = 1
    return
  end if

  call gamma_rat1 ( b, z, r, p, q, eps )

  v = 0.25D+00 * ( 1.0D+00 / nu )**2
  t2 = 0.25D+00 * lnx * lnx
  l = w / u
  j = q / r
  sum1 = j
  t = 1.0D+00
  cn = 1.0D+00
  n2 = 0.0D+00

  do n = 1, 30

    bp2n = b + n2
    j = ( bp2n * ( bp2n + 1.0D+00 ) * j &
      + ( z + bp2n + 1.0D+00 ) * t ) * v
    n2 = n2 +  2.0D+00
    t = t * t2
    cn = cn / ( n2 * ( n2 + 1.0D+00 ))
    c(n) = cn
    s = 0.0D+00

    coef = b - n
    do i = 1, n-1
      s = s + coef * c(i) * d(n-i)
      coef = coef + b
    end do

    d(n) = bm1 * cn + s / n
    dj = d(n) * j
    sum1 = sum1 + dj

    if ( sum1 <= 0.0D+00 ) then
      ierr = 1
      return
    end if

    if ( abs ( dj ) <= eps * ( sum1 + l ) ) then
      ierr = 0
      w = w + u * sum1
      return
    end if

  end do

  ierr = 0
  w = w + u * sum1

  return
end

subroutine gamma_rat1 ( a, x, r, p, q, eps )

!*****************************************************************************80
!
!! GAMMA_RAT1 evaluates the incomplete gamma ratio functions P(A,X) and Q(A,X).
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
!    Input, real ( kind = 8 ) A, X, the parameters of the functions.
!    It is assumed that A <= 1.
!
!    Input, real ( kind = 8 ) R, the value exp(-X) * X**A / Gamma(A).
!
!    Output, real ( kind = 8 ) P, Q, the values of P(A,X) and Q(A,X).
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2n
  real ( kind = 8 ) a2nm1
  real ( kind = 8 ) am0
  real ( kind = 8 ) an
  real ( kind = 8 ) an0
  real ( kind = 8 ) b2n
  real ( kind = 8 ) b2nm1
  real ( kind = 8 ) c
  real ( kind = 8 ) cma
  real ( kind = 8 ) eps
  real ( kind = 8 ) error_f
  real ( kind = 8 ) error_fc
  real ( kind = 8 ) g
  real ( kind = 8 ) gam1
  real ( kind = 8 ) h
  real ( kind = 8 ) j
  real ( kind = 8 ) l
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) r
  real ( kind = 8 ) rexp
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  if ( a * x == 0.0D+00 ) then

    if ( x <= a ) then
      p = 0.0D+00
      q = 1.0D+00
    else
      p = 1.0D+00
      q = 0.0D+00
    end if

    return
  end if

  if ( a == 0.5D+00 ) then

    if ( x < 0.25D+00 ) then
      p = error_f ( sqrt ( x ) )
      q = 0.5D+00 + ( 0.5D+00 - p )
    else
      q = error_fc ( 0, sqrt ( x ) )
      p = 0.5D+00 + ( 0.5D+00 - q )
    end if

    return

  end if
!
!  Taylor series for P(A,X)/X**A
!
  if ( x < 1.1D+00 ) then

    an = 3.0
    c = x
    sum1 = x / ( a + 3.0D+00 )
    tol = 0.1D+00 * eps / ( a + 1.0D+00 )

    do

      an = an + 1.0D+00
      c = -c * ( x / an )
      t = c / ( a + an )
      sum1 = sum1 + t

      if ( abs ( t ) <= tol ) then
        exit
      end if

    end do

    j = a * x * ( ( sum1 / 6.0D+00 - 0.5D+00 &
      / ( a +  2.0D+00  ) ) &
      * x + 1.0D+00 / ( a + 1.0D+00 ) )

    z = a * log ( x )
    h = gam1 ( a )
    g = 1.0D+00 + h

    if ( x < 0.25D+00 ) then
      go to 30
    end if

    if ( a < x / 2.59D+00 ) then
      go to 50
    else
      go to 40
    end if

30 continue

    if ( -0.13394D+00 < z ) then
      go to 50
    end if

40 continue

    w = exp ( z )
    p = w * g * ( 0.5D+00 + ( 0.5D+00 - j ))
    q = 0.5D+00 + ( 0.5D+00 - p )
    return

50 continue

    l = rexp ( z )
    w = 0.5D+00 + ( 0.5D+00 + l )
    q = ( w * j - l ) * g - h

    if  ( q < 0.0D+00 ) then
      p = 1.0D+00
      q = 0.0D+00
    else
      p = 0.5D+00 + ( 0.5D+00 - q )
    end if
!
!  Continued fraction expansion.
!
  else

    a2nm1 = 1.0D+00
    a2n = 1.0D+00
    b2nm1 = x
    b2n = x + ( 1.0D+00 - a )
    c = 1.0D+00

    do

      a2nm1 = x * a2n + c * a2nm1
      b2nm1 = x * b2n + c * b2nm1
      am0 = a2nm1 / b2nm1
      c = c + 1.0D+00
      cma = c - a
      a2n = a2nm1 + cma * a2n
      b2n = b2nm1 + cma * b2n
      an0 = a2n / b2n

      if ( abs ( an0 - am0 ) < eps * an0 ) then
        exit
      end if

    end do

    q = r * an0
    p = 0.5D+00 + ( 0.5D+00 - q )

  end if

  return
end

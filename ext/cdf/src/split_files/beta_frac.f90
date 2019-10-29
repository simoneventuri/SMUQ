function beta_frac ( a, b, x, y, lambda, eps )

!*****************************************************************************80
!
!! BETA_FRAC evaluates a continued fraction expansion for IX(A,B).
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
!    A and B should be nonnegative.  It is assumed that both A and
!    B are greater than 1.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Input, real ( kind = 8 ) LAMBDA, the value of ( A + B ) * Y - B.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) BETA_FRAC, the value of the continued
!    fraction approximation for IX(A,B).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alpha
  real ( kind = 8 ) an
  real ( kind = 8 ) anp1
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_frac
  real ( kind = 8 ) beta_rcomp
  real ( kind = 8 ) bn
  real ( kind = 8 ) bnp1
  real ( kind = 8 ) c
  real ( kind = 8 ) c0
  real ( kind = 8 ) c1
  real ( kind = 8 ) e
  real ( kind = 8 ) eps
  real ( kind = 8 ) lambda
  real ( kind = 8 ) n
  real ( kind = 8 ) p
  real ( kind = 8 ) r
  real ( kind = 8 ) r0
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) yp1

  beta_frac = beta_rcomp ( a, b, x, y )

  if ( beta_frac == 0.0D+00 ) then
    return
  end if

  c = 1.0D+00 + lambda
  c0 = b / a
  c1 = 1.0D+00 + 1.0D+00 / a
  yp1 = y + 1.0D+00

  n = 0.0D+00
  p = 1.0D+00
  s = a + 1.0D+00
  an = 0.0D+00
  bn = 1.0D+00
  anp1 = 1.0D+00
  bnp1 = c / c1
  r = c1 / c
!
!  Continued fraction calculation.
!
  do

    n = n + 1.0D+00
    t = n / a
    w = n * ( b - n ) * x
    e = a / s
    alpha = ( p * ( p + c0 ) * e * e ) * ( w * x )
    e = ( 1.0D+00 + t ) / ( c1 + t + t )
    beta = n + w / s + e * ( c + n * yp1 )
    p = 1.0D+00 + t
    s = s +  2.0D+00
!
!  Update AN, BN, ANP1, and BNP1.
!
    t = alpha * an + beta * anp1
    an = anp1
    anp1 = t
    t = alpha * bn + beta * bnp1
    bn = bnp1
    bnp1 = t

    r0 = r
    r = anp1 / bnp1

    if ( abs ( r - r0 ) <= eps * r ) then
      beta_frac = beta_frac * r
      exit
    end if
!
!  Rescale AN, BN, ANP1, and BNP1.
!
    an = an / bnp1
    bn = bn / bnp1
    anp1 = r
    bnp1 = 1.0D+00

  end do

  return
end

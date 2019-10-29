function beta_rcomp1 ( mu, a, b, x, y )

!*****************************************************************************80
!
!! BETA_RCOMP1 evaluates exp(MU) * X^A * Y^B / Beta(A,B).
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
!    Input, integer ( kind = 4 ) MU, ?
!
!    Input, real ( kind = 8 ) A, B, the parameters of the Beta function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y, quantities whose powers form part of
!    the expression.
!
!    Output, real ( kind = 8 ) BETA_RCOMP1, the value of
!    exp(MU) * X**A * Y**B / Beta(A,B).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) apb
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) beta_rcomp1
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: const = 0.398942280401433D+00
  real ( kind = 8 ) e
  real ( kind = 8 ) esum
  real ( kind = 8 ) gam1
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) lambda
  real ( kind = 8 ) lnx
  real ( kind = 8 ) lny
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ) rlog1
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) x
  real ( kind = 8 ) x0
  real ( kind = 8 ) y
  real ( kind = 8 ) y0
  real ( kind = 8 ) z

  a0 = min ( a, b )
!
!  Procedure for 8 <= A and 8 <= B.
!
  if ( 8.0D+00 <= a0 ) then

    if ( a <= b ) then
      h = a / b
      x0 = h / ( 1.0D+00 + h )
      y0 = 1.0D+00 / ( 1.0D+00 + h )
      lambda = a - ( a + b ) * x
    else
      h = b / a
      x0 = 1.0D+00 / ( 1.0D+00 + h )
      y0 = h / ( 1.0D+00 + h )
      lambda = ( a + b ) * y - b
    end if

    e = -lambda / a

    if ( abs ( e ) <= 0.6D+00 ) then
      u = rlog1 ( e )
    else
      u = e - log ( x / x0 )
    end if

    e = lambda / b

    if ( abs ( e ) <= 0.6D+00 ) then
      v = rlog1 ( e )
    else
      v = e - log ( y / y0 )
    end if

    z = esum ( mu, - ( a * u + b * v ))
    beta_rcomp1 = const * sqrt ( b * x0 ) * z * exp ( - bcorr ( a, b ) )
!
!  Procedure for A < 8 or B < 8.
!
  else

    if ( x <= 0.375D+00 ) then
      lnx = log ( x )
      lny = alnrel ( - x )
    else if ( y <= 0.375D+00 ) then
      lnx = alnrel ( - y )
      lny = log ( y )
    else
      lnx = log ( x )
      lny = log ( y )
    end if

    z = a * lnx + b * lny

    if ( 1.0D+00 <= a0 ) then
      z = z - beta_log ( a, b )
      beta_rcomp1 = esum ( mu, z )
      return
    end if
!
!  Procedure for A < 1 or B < 1.
!
    b0 = max ( a, b )

    if ( 8.0D+00 <= b0 ) then
      u = gamma_ln1 ( a0 ) + algdiv ( a0, b0 )
      beta_rcomp1 = a0 * esum ( mu, z-u )
      return
    end if

    if ( 1.0D+00 < b0 ) then
!
!  Algorithm for 1 < B0 < 8
!
      u = gamma_ln1 ( a0 )
      n = b0 - 1.0D+00

      c = 1.0D+00
      do i = 1, n
        b0 = b0 - 1.0D+00
        c = c * ( b0 / ( a0 + b0 ) )
      end do
      u = log ( c ) + u

      z = z - u
      b0 = b0 - 1.0D+00
      apb = a0 + b0

      if ( apb <= 1.0D+00 ) then
        t = 1.0D+00 + gam1 ( apb )
      else
        u = a0 + b0 - 1.0D+00
        t = ( 1.0D+00 + gam1 ( u ) ) / apb
      end if

      beta_rcomp1 = a0 * esum ( mu, z ) &
        * ( 1.0D+00 + gam1 ( b0 ) ) / t
!
!  Algorithm for B0 <= 1
!
    else

      beta_rcomp1 = esum ( mu, z )
      if ( beta_rcomp1 == 0.0D+00 ) then
        return
      end if

      apb = a + b

      if ( apb <= 1.0D+00 ) then
        z = 1.0D+00 + gam1 ( apb )
      else
        u = real ( a, kind = 8 ) + real ( b, kind = 8 ) - 1.0D+00
        z = ( 1.0D+00 + gam1 ( u )) / apb
      end if

      c = ( 1.0D+00 + gam1 ( a ) ) &
        * ( 1.0D+00 + gam1 ( b ) ) / z
      beta_rcomp1 = beta_rcomp1 * ( a0 * c ) / ( 1.0D+00 + a0 / b0 )

    end if

  end if

  return
end

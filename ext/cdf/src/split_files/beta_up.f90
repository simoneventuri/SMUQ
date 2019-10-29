function beta_up ( a, b, x, y, n, eps )

!*****************************************************************************80
!
!! BETA_UP evaluates IX(A,B) - IX(A+N,B) where N is a positive integer.
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
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y, ?
!
!    Input, integer ( kind = 4 ) N, the increment to the first argument of IX.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
!    Output, real ( kind = 8 ) BETA_UP, the value of IX(A,B) - IX(A+N,B).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) ap1
  real ( kind = 8 ) apb
  real ( kind = 8 ) b
  real ( kind = 8 ) beta_rcomp1
  real ( kind = 8 ) beta_up
  real ( kind = 8 ) d
  real ( kind = 8 ) eps
  real ( kind = 8 ) exparg
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real ( kind = 8 ) l
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) n
  real ( kind = 8 ) r
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) y
!
!  Obtain the scaling factor EXP(-MU) AND
!  EXP(MU)*(X**A*Y**B/BETA(A,B))/A
!
  apb = a + b
  ap1 = a + 1.0D+00
  mu = 0
  d = 1.0D+00

  if ( n /= 1 ) then

    if ( 1.0D+00 <= a ) then

      if ( 1.1D+00 * ap1 <= apb ) then
        mu = abs ( exparg ( 1 ) )
        k = exparg ( 0 )
        if ( k < mu ) then
          mu = k
        end if
        t = mu
        d = exp ( - t )
      end if

    end if

  end if

  beta_up = beta_rcomp1 ( mu, a, b, x, y ) / a

  if ( n == 1 .or. beta_up == 0.0D+00 ) then
    return
  end if

  w = d
!
!  Let K be the index of the maximum term.
!
  k = 0

  if ( 1.0D+00 < b ) then

    if ( y <= 0.0001D+00 ) then

      k = n - 1

    else

      r = ( b - 1.0D+00 ) * x / y - a

      if ( 1.0D+00 <= r ) then
        k = n - 1
        t = n - 1
        if ( r < t ) then
          k = r
        end if
      end if

    end if
!
!  Add the increasing terms of the series.
!
    do i = 1, k
      l = i - 1
      d = ( ( apb + l ) / ( ap1 + l ) ) * x * d
      w = w + d
    end do

  end if
!
!  Add the remaining terms of the series.
!
  do i = k+1, n-1
    l = i - 1
    d = ( ( apb + l ) / ( ap1 + l ) ) * x * d
    w = w + d
    if ( d <= eps * w ) then
      beta_up = beta_up * w
      return
    end if
  end do

  beta_up = beta_up * w

  return
end

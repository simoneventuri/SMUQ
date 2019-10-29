function beta_pser ( a, b, x, eps )

!*****************************************************************************80
!
!! BETA_PSER uses a power series expansion to evaluate IX(A,B)(X).
!
!  Discussion:
!
!    BETA_PSER is used when B <= 1 or B*X <= 0.7.
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
!    Input, real ( kind = 8 ) A, B, the parameters.
!
!    Input, real ( kind = 8 ) X, the point where the function
!    is to be evaluated.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
!    Output, real ( kind = 8 ) BETA_PSER, the approximate value of IX(A,B)(X).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) apb
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) beta_pser
  real ( kind = 8 ) c
  real ( kind = 8 ) eps
  real ( kind = 8 ) gam1
  real ( kind = 8 ) gamma_ln1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ) n
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) u
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  beta_pser = 0.0D+00

  if ( x == 0.0D+00 ) then
    return
  end if
!
!  Compute the factor X**A/(A*BETA(A,B))
!
  a0 = min ( a, b )

  if ( 1.0D+00 <= a0 ) then

    z = a * log ( x ) - beta_log ( a, b )
    beta_pser = exp ( z ) / a

  else

    b0 = max ( a, b )

    if ( b0 <= 1.0D+00 ) then

      beta_pser = x**a
      if ( beta_pser == 0.0D+00 ) then
        return
      end if

      apb = a + b

      if ( apb <= 1.0D+00 ) then
        z = 1.0D+00 + gam1 ( apb )
      else
        u = a + b - 1.0D+00
        z = ( 1.0D+00 + gam1 ( u ) ) / apb
      end if

      c = ( 1.0D+00 + gam1 ( a ) ) &
        * ( 1.0D+00 + gam1 ( b ) ) / z
      beta_pser = beta_pser * c * ( b / apb )

    else if ( b0 < 8.0D+00 ) then

      u = gamma_ln1 ( a0 )
      m = b0 - 1.0D+00

      c = 1.0D+00
      do i = 1, m
        b0 = b0 - 1.0D+00
        c = c * ( b0 / ( a0 + b0 ))
      end do

      u = log ( c ) + u
      z = a * log ( x ) - u
      b0 = b0 - 1.0D+00
      apb = a0 + b0

      if ( apb <= 1.0D+00 ) then
        t = 1.0D+00 + gam1 ( apb )
      else
        u = a0 + b0 - 1.0D+00
        t = ( 1.0D+00 + gam1 ( u ) ) / apb
      end if

      beta_pser = exp ( z ) * ( a0 / a ) &
        * ( 1.0D+00 + gam1 ( b0 )) / t

    else if ( 8.0D+00 <= b0 ) then

      u = gamma_ln1 ( a0 ) + algdiv ( a0, b0 )
      z = a * log ( x ) - u
      beta_pser = ( a0 / a ) * exp ( z )

    end if

  end if

  if ( beta_pser == 0.0D+00 .or. a <= 0.1D+00 * eps ) then
    return
  end if
!
!  Compute the series.
!
  sum1 = 0.0D+00
  n = 0.0D+00
  c = 1.0D+00
  tol = eps / a

  do

    n = n + 1.0D+00
    c = c * ( 0.5D+00 + ( 0.5D+00 - b / n ) ) * x
    w = c / ( a + n )
    sum1 = sum1 + w

    if ( abs ( w ) <= tol ) then
      exit
    end if

  end do

  beta_pser = beta_pser * ( 1.0D+00 + a * sum1 )

  return
end

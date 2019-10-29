function beta_log ( a0, b0 )

!*****************************************************************************80
!
!! BETA_LOG evaluates the logarithm of the beta function.
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
!    Input, real ( kind = 8 ) A0, B0, the parameters of the function.
!    A0 and B0 should be nonnegative.
!
!    Output, real ( kind = 8 ) BETA_LOG, the value of the logarithm
!    of the Beta function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) beta_log
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: e = 0.918938533204673D+00
  real ( kind = 8 ) gamma_log
  real ( kind = 8 ) gsumln
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) z

  a = min ( a0, b0 )
  b = max ( a0, b0 )
!
!  8 < A.
!
  if ( 8.0D+00 <= a ) then

    w = bcorr ( a, b )
    h = a / b
    c = h / ( 1.0D+00 + h )
    u = - ( a - 0.5D+00 ) * log ( c )
    v = b * alnrel ( h )

    if ( v < u ) then
      beta_log = ((( -0.5D+00 * log ( b ) + e ) + w ) - v ) - u
    else
      beta_log = ((( -0.5D+00 * log ( b ) + e ) + w ) - u ) - v
    end if

    return
  end if
!
!  Procedure when A < 1
!
  if ( a < 1.0D+00 ) then

    if ( b < 8.0D+00 ) then
      beta_log = gamma_log ( a ) + ( gamma_log ( b ) - gamma_log ( a + b ) )
    else
      beta_log = gamma_log ( a ) + algdiv ( a, b )
    end if

    return

  end if
!
!  Procedure when 1 <= A < 8
!
  if ( 2.0D+00 < a ) then
    go to 40
  end if

  if ( b <= 2.0D+00 ) then
    beta_log = gamma_log ( a ) + gamma_log ( b ) - gsumln ( a, b )
    return
  end if

  w = 0.0D+00

  if ( b < 8.0D+00 ) then
    go to 60
  end if

  beta_log = gamma_log ( a ) + algdiv ( a, b )
  return

40 continue
!
!  Reduction of A when 1000 < B.
!
  if ( 1000.0D+00 < b ) then

    n = a - 1.0D+00
    w = 1.0D+00
    do i = 1, n
      a = a - 1.0D+00
      w = w * ( a / ( 1.0D+00 + a / b ))
    end do

    beta_log = ( log ( w ) - n * log ( b ) ) &
      + ( gamma_log ( a ) + algdiv ( a, b ) )

    return
  end if

  n = a - 1.0D+00
  w = 1.0D+00
  do i = 1, n
    a = a - 1.0D+00
    h = a / b
    w = w * ( h / ( 1.0D+00 + h ) )
  end do
  w = log ( w )

  if ( 8.0D+00 <= b ) then
    beta_log = w + gamma_log ( a ) + algdiv ( a, b )
    return
  end if
!
!  Reduction of B when B < 8.
!
60 continue

  n = b - 1.0D+00
  z = 1.0D+00
  do i = 1, n
    b = b - 1.0D+00
    z = z * ( b / ( a + b ))
  end do

  beta_log = w + log ( z ) + ( gamma_log ( a ) + ( gamma_log ( b ) &
    - gsumln ( a, b ) ) )

  return
end

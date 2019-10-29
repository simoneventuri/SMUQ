function algdiv ( a, b )

!*****************************************************************************80
!
!! ALGDIV computes ln ( Gamma ( B ) / Gamma ( A + B ) ) when 8 <= B.
!
!  Discussion:
!
!    In this algorithm, DEL(X) is the function defined by
!
!      ln ( Gamma(X) ) = ( X - 0.5 ) * ln ( X ) - X + 0.5 * ln ( 2 * PI )
!                      + DEL(X).
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
!    Input, real ( kind = 8 ) A, B, define the arguments.
!
!    Output, real ( kind = 8 ) ALGDIV, the value of ln(Gamma(B)/Gamma(A+B)).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) algdiv
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: c0 =  0.833333333333333D-01
  real ( kind = 8 ), parameter :: c1 = -0.277777777760991D-02
  real ( kind = 8 ), parameter :: c2 =  0.793650666825390D-03
  real ( kind = 8 ), parameter :: c3 = -0.595202931351870D-03
  real ( kind = 8 ), parameter :: c4 =  0.837308034031215D-03
  real ( kind = 8 ), parameter :: c5 = -0.165322962780713D-02
  real ( kind = 8 ) d
  real ( kind = 8 ) h
  real ( kind = 8 ) s11
  real ( kind = 8 ) s3
  real ( kind = 8 ) s5
  real ( kind = 8 ) s7
  real ( kind = 8 ) s9
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  if ( b < a ) then
    h = b / a
    c = 1.0D+00 / ( 1.0D+00 + h )
    x = h / ( 1.0D+00 + h )
    d = a + ( b - 0.5D+00 )
  else
    h = a / b
    c = h / ( 1.0D+00 + h )
    x = 1.0D+00 / ( 1.0D+00 + h )
    d = b + ( a - 0.5D+00 )
  end if
!
!  Set SN = (1 - X^N)/(1 - X).
!
  x2 = x * x
  s3 = 1.0D+00 + ( x + x2 )
  s5 = 1.0D+00 + ( x + x2 * s3 )
  s7 = 1.0D+00 + ( x + x2 * s5 )
  s9 = 1.0D+00 + ( x + x2 * s7 )
  s11 = 1.0D+00 + ( x + x2 * s9 )
!
!  Set W = DEL(B) - DEL(A + B).
!
  t = ( 1.0D+00 / b )**2
  w = (((( &
          c5 * s11  * t &
        + c4 * s9 ) * t &
        + c3 * s7 ) * t &
        + c2 * s5 ) * t &
        + c1 * s3 ) * t &
        + c0

  w = w * ( c / b )
!
!  Combine the results.
!
  u = d * alnrel ( a / b )
  v = a * ( log ( b ) - 1.0D+00 )

  if ( v < u ) then
    algdiv = ( w - v ) - u
  else
    algdiv = ( w - u ) - v
  end if

  return
end

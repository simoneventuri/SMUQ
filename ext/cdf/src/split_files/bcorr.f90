function bcorr ( a0, b0 )

!*****************************************************************************80
!
!! BCORR evaluates DEL(A0) + DEL(B0) - DEL(A0 + B0).
!
!  Discussion:
!
!    The function DEL(A) is a remainder term that is used in the expression:
!
!      ln ( Gamma ( A ) ) = ( A - 0.5 ) * ln ( A )
!        - A + 0.5 * ln ( 2 * PI ) + DEL ( A ),
!
!    or, in other words, DEL ( A ) is defined as:
!
!      DEL ( A ) = ln ( Gamma ( A ) ) - ( A - 0.5 ) * ln ( A )
!        + A + 0.5 * ln ( 2 * PI ).
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
!    Input, real ( kind = 8 ) A0, B0, the arguments.
!    It is assumed that 8 <= A0 and 8 <= B0.
!
!    Output, real ( kind = 8 ) BCORR, the value of the function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: c0 =  0.833333333333333D-01
  real ( kind = 8 ), parameter :: c1 = -0.277777777760991D-02
  real ( kind = 8 ), parameter :: c2 =  0.793650666825390D-03
  real ( kind = 8 ), parameter :: c3 = -0.595202931351870D-03
  real ( kind = 8 ), parameter :: c4 =  0.837308034031215D-03
  real ( kind = 8 ), parameter :: c5 = -0.165322962780713D-02
  real ( kind = 8 ) h
  real ( kind = 8 ) s11
  real ( kind = 8 ) s3
  real ( kind = 8 ) s5
  real ( kind = 8 ) s7
  real ( kind = 8 ) s9
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  a = min ( a0, b0 )
  b = max ( a0, b0 )

  h = a / b
  c = h / ( 1.0D+00 + h )
  x = 1.0D+00 / ( 1.0D+00 + h )
  x2 = x * x
!
!  Set SN = (1 - X**N)/(1 - X)
!
  s3 = 1.0D+00 + ( x + x2 )
  s5 = 1.0D+00 + ( x + x2 * s3 )
  s7 = 1.0D+00 + ( x + x2 * s5 )
  s9 = 1.0D+00 + ( x + x2 * s7 )
  s11 = 1.0D+00 + ( x + x2 * s9 )
!
!  Set W = DEL(B) - DEL(A + B)
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
!  Compute  DEL(A) + W.
!
  t = ( 1.0D+00 / a )**2

  bcorr = ((((( &
         c5   * t &
       + c4 ) * t &
       + c3 ) * t &
       + c2 ) * t &
       + c1 ) * t &
       + c0 ) / a + w

  return
end

function gsumln ( a, b )

!*****************************************************************************80
!
!! GSUMLN evaluates the function ln(Gamma(A + B)).
!
!  Discussion:
!
!    GSUMLN is used for 1 <= A <= 2 and 1 <= B <= 2
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
!    Input, real ( kind = 8 ) A, B, values whose sum is the argument of
!    the Gamma function.
!
!    Output, real ( kind = 8 ) GSUMLN, the value of ln(Gamma(A+B)).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alnrel
  real ( kind = 8 ) b
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ) gsumln
  real ( kind = 8 ) x

  x = a + b - 2.0D+00

  if ( x <= 0.25D+00 ) then
    gsumln = gamma_ln1 ( 1.0D+00 + x )
  else if ( x <= 1.25D+00 ) then
    gsumln = gamma_ln1 ( x ) + alnrel ( x )
  else
    gsumln = gamma_ln1 ( x - 1.0D+00 ) + log ( x * ( 1.0D+00 + x ) )
  end if

  return
end

function dbetrm ( a, b )

!*****************************************************************************80
!
!! DBETRM computes the Sterling remainder for the complete beta function.
!
!  Discussion:
!
!    Log(Beta(A,B)) = Lgamma(A) + Lgamma(B) - Lgamma(A+B)
!    where Lgamma is the log of the (complete) gamma function
!
!    Let ZZ be approximation obtained if each log gamma is approximated
!    by Sterling's formula, i.e.,
!
!      Sterling(Z) = log ( sqrt ( 2 * PI ) ) + ( Z - 0.5 ) * log ( Z ) - Z
!
!    The Sterling remainder is Log(Beta(A,B)) - ZZ.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the Beta function.
!
!    Output, real ( kind = 8 ) DBETRM, the Sterling remainder.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) dbetrm
  real ( kind = 8 ) dstrem
!
!  Try to sum from smallest to largest.
!
  dbetrm = -dstrem ( a + b )
  dbetrm = dbetrm + dstrem ( max ( a, b ) )
  dbetrm = dbetrm + dstrem ( min ( a, b ) )

  return
end

function r8_beta ( a, b )

!*****************************************************************************80
!
!! R8_BETA returns the value of the Beta function.
!
!  Discussion:
!
!    The Beta function is defined as
!
!      BETA(A,B) = ( GAMMA ( A ) * GAMMA ( B ) ) / GAMMA ( A + B )
!                = Integral ( 0 <= T <= 1 ) T^(A-1) (1-T)^(B-1) dT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) R8_BETA, the value of the function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) r8_beta

  if ( a <= 0.0D+00 .or. b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_BETA - Fatal error!'
    write ( *, '(a)' ) '  Both A and B must be greater than 0.'
    stop 1
  end if

  r8_beta = exp ( lgamma ( a ) + lgamma ( b ) - lgamma ( a + b ) )

  return
end

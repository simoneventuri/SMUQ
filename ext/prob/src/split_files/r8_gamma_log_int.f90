function r8_gamma_log_int ( n )

!*****************************************************************************80
!
!! R8_GAMMA_LOG_INT computes the logarithm of Gamma of an integer N.
!
!  Discussion:
!
!    log ( n! ) = r8_gamma_log_int ( n + 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the logarithm of the
!    Gamma function.  0 < N.
!
!    Output, real ( kind = 8 ) R8_GAMMA_LOG_INT, the logarithm of
!    the Gamma function of N.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) r8_gamma_log_int

  if ( n <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_GAMMA_LOG_INT - Fatal error!'
    write ( *, '(a,i12)' ) '  Illegal input value of N = ', n
    write ( *, '(a)' ) '  But N must be strictly positive.'
    stop 1
  end if

  r8_gamma_log_int = lgamma ( real ( n, kind = 8 ) )

  return
end

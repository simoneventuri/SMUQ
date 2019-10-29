function beta ( a, b )

!*****************************************************************************80
!
!! BETA evaluates the beta function.
!
!  Modified:
!
!    03 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the arguments of the beta function.
!
!    Output, real ( kind = 8 ) BETA, the value of the beta function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) beta
  real ( kind = 8 ) beta_log

  beta = exp ( beta_log ( a, b ) )

  return
end

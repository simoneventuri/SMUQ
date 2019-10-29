function dqwgtc ( x, c, p2, p3, p4, kp )

!*****************************************************************************80
!
!! DQWGTC defines the weight function used by DQC25C.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
  implicit none

  real ( kind = 8 ) dqwgtc
  real ( kind = 8 ) c,p2,p3,p4,x
  integer ( kind = 4 ) kp

  dqwgtc = 0.1D+01 / ( x - c )

  return
end

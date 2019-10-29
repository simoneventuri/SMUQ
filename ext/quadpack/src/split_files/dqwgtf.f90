function dqwgtf(x,omega,p2,p3,p4,integr)

!*****************************************************************************80
!
!! DQWGTF defines the weight functions used by DQC25F.
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

  real ( kind = 8 ) dqwgtf
  real ( kind = 8 ) dcos,dsin,omega,omx,p2,p3,p4,x
  integer ( kind = 4 ) integr

  omx = omega * x

  if ( integr == 1 ) then
    dqwgtf = cos ( omx )
  else
    dqwgtf = sin ( omx )
  end if

  return
end

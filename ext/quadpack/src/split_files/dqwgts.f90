function dqwgts ( x, a, b, alfa, beta, integr )

!*****************************************************************************80
!
!! DQWGTS defines the weight functions used by DQC25S.
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

  real dqwgts
  real ( kind = 8 ) a,alfa,b,beta,bmx,x,xma
  integer ( kind = 4 ) integr

  xma = x - a
  bmx = b - x
  dqwgts = xma ** alfa * bmx ** beta
  go to (40,10,20,30),integr
   10 dqwgts = dqwgts* log ( xma )
  go to 40
   20 dqwgts = dqwgts* log ( bmx )
  go to 40
   30 dqwgts = dqwgts* log ( xma ) * log ( bmx )
   40 continue

  return
end

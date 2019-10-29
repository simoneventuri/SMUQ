subroutine dqmomo(alfa,beta,ri,rj,rg,rh,integr)

!*****************************************************************************80
!
!! DQMOMO computes modified Chebyshev moments.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  this routine computes modified chebsyshev moments. the k-th
!      modified chebyshev moment is defined as the integral over
!      (-1,1) of w(x)*t(k,x), where t(k,x) is the chebyshev
!      polynomial of degree k.
!
!  Parameters:
!
!     alfa   - real ( kind = 8 )
!              parameter in the weight function w(x), alfa.gt.(-1)
!
!     beta   - real ( kind = 8 )
!              parameter in the weight function w(x), beta.gt.(-1)
!
!     ri     - real ( kind = 8 )
!              vector of dimension 25
!              ri(k) is the integral over (-1,1) of
!              (1+x)**alfa*t(k-1,x), k = 1, ..., 25.
!
!     rj     - real ( kind = 8 )
!              vector of dimension 25
!              rj(k) is the integral over (-1,1) of
!              (1-x)**beta*t(k-1,x), k = 1, ..., 25.
!
!     rg     - real ( kind = 8 )
!              vector of dimension 25
!              rg(k) is the integral over (-1,1) of
!              (1+x)**alfa*log((1+x)/2)*t(k-1,x), k = 1, ..., 25.
!
!     rh     - real ( kind = 8 )
!              vector of dimension 25
!              rh(k) is the integral over (-1,1) of
!              (1-x)**beta*log((1-x)/2)*t(k-1,x), k = 1, ..., 25.
!
!     integr - integer ( kind = 4 )
!              input parameter indicating the modified
!              moments to be computed
!              integr = 1 compute ri, rj
!                     = 2 compute ri, rj, rg
!                     = 3 compute ri, rj, rh
!                     = 4 compute ri, rj, rg, rh
!
  implicit none

  real ( kind = 8 ) alfa,alfp1,alfp2,an,anm1,beta,betp1,betp2,ralf, &
    rbet,rg,rh,ri,rj
  integer ( kind = 4 ) i,im1,integr
  dimension rg(25),rh(25),ri(25),rj(25)

  alfp1 = alfa+0.1D+01
  betp1 = beta+0.1D+01
  alfp2 = alfa+0.2D+01
  betp2 = beta+0.2D+01
  ralf = 0.2D+01**alfp1
  rbet = 0.2D+01**betp1
!
!  compute ri, rj using a forward recurrence relation.
!
  ri(1) = ralf/alfp1
  rj(1) = rbet/betp1
  ri(2) = ri(1)*alfa/alfp2
  rj(2) = rj(1)*beta/betp2
  an = 0.2D+01
  anm1 = 0.1D+01

  do i=3,25
    ri(i) = -(ralf+an*(an-alfp2)*ri(i-1))/(anm1*(an+alfp1))
    rj(i) = -(rbet+an*(an-betp2)*rj(i-1))/(anm1*(an+betp1))
    anm1 = an
    an = an+0.1D+01
  end do

  if(integr.eq.1) go to 70
  if(integr.eq.3) go to 40
!
!  compute rg using a forward recurrence relation.
!
  rg(1) = -ri(1)/alfp1
  rg(2) = -(ralf+ralf)/(alfp2*alfp2)-rg(1)
  an = 0.2D+01
  anm1 = 0.1D+01
  im1 = 2

  do i=3,25
    rg(i) = -(an*(an-alfp2)*rg(im1)-an*ri(im1)+anm1*ri(i))/ &
    (anm1*(an+alfp1))
    anm1 = an
    an = an+0.1D+01
    im1 = i
  end do

  if(integr.eq.2) go to 70
!
!  compute rh using a forward recurrence relation.
!
   40 rh(1) = -rj(1)/betp1
  rh(2) = -(rbet+rbet)/(betp2*betp2)-rh(1)
  an = 0.2D+01
  anm1 = 0.1D+01
  im1 = 2

  do i=3,25
    rh(i) = -(an*(an-betp2)*rh(im1)-an*rj(im1)+ &
    anm1*rj(i))/(anm1*(an+betp1))
    anm1 = an
    an = an+0.1D+01
    im1 = i
  end do

  do i=2,25,2
    rh(i) = -rh(i)
  end do

   70 continue

  do i=2,25,2
    rj(i) = -rj(i)
  end do

   90 continue

  return
end

subroutine dqc25s(f,a,b,bl,br,alfa,beta,ri,rj,rg,rh,result, &
     abserr,resasc,integr,nev)

!*****************************************************************************80
!
!! DQC25S returns rules for algebraico-logarithmic end point singularities.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  to compute i = integral of f*w over (bl,br), with error
!      estimate, where the weight function w has a singular
!      behaviour of algebraico-logarithmic type at the points
!      a and/or b. (bl,br) is a part of (a,b).
!
!  Parameters:
!
!     f      - real ( kind = 8 )
!              function subprogram defining the integrand
!              f(x). the actual name for f needs to be declared
!              e x t e r n a l  in the driver program.
!
!     a      - real ( kind = 8 )
!              left end point of the original interval
!
!     b      - real ( kind = 8 )
!              right end point of the original interval, b.gt.a
!
!     bl     - real ( kind = 8 )
!              lower limit of integration, bl.ge.a
!
!     br     - real ( kind = 8 )
!              upper limit of integration, br.le.b
!
!     alfa   - real ( kind = 8 )
!              parameter in the weight function
!
!     beta   - real ( kind = 8 )
!              parameter in the weight function
!
!     ri,rj,rg,rh - real ( kind = 8 )
!              modified chebyshev moments for the application
!              of the generalized clenshaw-curtis
!              method (computed in routine dqmomo)
!
!     result - real ( kind = 8 )
!              approximation to the integral
!              result is computed by using a generalized
!              clenshaw-curtis method if b1 = a or br = b.
!              in all other cases the 15-point kronrod
!              rule is applied, obtained by optimal addition of
!              abscissae to the 7-point gauss rule.
!
!     abserr - real ( kind = 8 )
!              estimate of the modulus of the absolute error,
!              which should equal or exceed abs(i-result)
!
!     resasc - real ( kind = 8 )
!              approximation to the integral of abs(f*w-i/(b-a))
!
!     integr - integer ( kind = 4 )
!              which determines the weight function
!              = 1   w(x) = (x-a)**alfa*(b-x)**beta
!              = 2   w(x) = (x-a)**alfa*(b-x)**beta*log(x-a)
!              = 3   w(x) = (x-a)**alfa*(b-x)**beta*log(b-x)
!              = 4   w(x) = (x-a)**alfa*(b-x)**beta*log(x-a)*
!                           log(b-x)
!
!     nev    - integer ( kind = 4 )
!              number of integrand evaluations
!
!  Local Parameters:
!
!     the vector x contains the values cos(k*pi/24)
!     k = 1, ..., 11, to be used for the computation of the
!     chebyshev series expansion of f.
!
!     fval   - value of the function f at the points
!              (br-bl)*0.5*cos(k*pi/24)+(br+bl)*0.5
!              k = 0, ..., 24
!     cheb12 - coefficients of the chebyshev series expansion
!              of degree 12, for the function f, in the
!              interval (bl,br)
!     cheb24 - coefficients of the chebyshev series expansion
!              of degree 24, for the function f, in the
!              interval (bl,br)
!     res12  - approximation to the integral obtained from cheb12
!     res24  - approximation to the integral obtained from cheb24
!     dqwgts - external function subprogram defining
!              the four possible weight functions
!     hlgth  - half-length of the interval (bl,br)
!     centr  - mid point of the interval (bl,br)
!
  implicit none

  real ( kind = 8 ) a,abserr,alfa,b,beta,bl,br,centr,cheb12,cheb24, &
    dc,f,factor,fix,fval,hlgth,resabs,resasc,result,res12, &
    res24,rg,rh,ri,rj,u,dqwgts,x
  integer ( kind = 4 ) i,integr,isym,nev

  dimension cheb12(13),cheb24(25),fval(25),rg(25),rh(25),ri(25), &
    rj(25),x(11)

  external f,dqwgts

  data x(1) / 0.991444861373810411144557526928563d0 /
  data x(2) / 0.965925826289068286749743199728897d0 /
  data x(3) / 0.923879532511286756128183189396788d0 /
  data x(4) / 0.866025403784438646763723170752936d0 /
  data x(5) / 0.793353340291235164579776961501299d0 /
  data x(6) / 0.707106781186547524400844362104849d0 /
  data x(7) / 0.608761429008720639416097542898164d0 /
  data x(8) / 0.500000000000000000000000000000000d0 /
  data x(9) / 0.382683432365089771728459984030399d0 /
  data x(10) / 0.258819045102520762348898837624048d0 /
  data x(11) / 0.130526192220051591548406227895489d0 /

  nev = 25
  if(bl.eq.a.and.(alfa.ne.0.0D+00.or.integr.eq.2.or.integr.eq.4)) &
   go to 10
  if(br.eq.b.and.(beta.ne.0.0D+00.or.integr.eq.3.or.integr.eq.4)) &
   go to 140
!
!  if a.gt.bl and b.lt.br, apply the 15-point gauss-kronrod scheme.
!
!
  call dqk15w(f,dqwgts,a,b,alfa,beta,integr,bl,br, &
      result,abserr,resabs,resasc)
  nev = 15
  go to 270
!
!  this part of the program is executed only if a = bl.
!
!  compute the chebyshev series expansion of the
!  following function
!  f1 = (0.5*(b+b-br-a)-0.5*(br-a)*x)**beta
!         *f(0.5*(br-a)*x+0.5*(br+a))
!
   10 hlgth = 0.5D+00*(br-bl)
  centr = 0.5D+00*(br+bl)
  fix = b-centr
  fval(1) = 0.5D+00*f(hlgth+centr)*(fix-hlgth)**beta
  fval(13) = f(centr)*(fix**beta)
  fval(25) = 0.5D+00*f(centr-hlgth)*(fix+hlgth)**beta
  do i=2,12
    u = hlgth*x(i-1)
    isym = 26-i
    fval(i) = f(u+centr)*(fix-u)**beta
    fval(isym) = f(centr-u)*(fix+u)**beta
  end do

  factor = hlgth**(alfa+0.1D+01)
  result = 0.0D+00
  abserr = 0.0D+00
  res12 = 0.0D+00
  res24 = 0.0D+00
  if(integr.gt.2) go to 70
  call dqcheb(x,fval,cheb12,cheb24)
!
!  integr = 1  (or 2)
!
  do i=1,13
    res12 = res12+cheb12(i)*ri(i)
    res24 = res24+cheb24(i)*ri(i)
  end do

  do i=14,25
    res24 = res24+cheb24(i)*ri(i)
  end do

  if(integr.eq.1) go to 130
!
!  integr = 2
!
  dc = log (br-bl)
  result = res24*dc
  abserr =  abs ( (res24-res12)*dc)
  res12 = 0.0D+00
  res24 = 0.0D+00
  do i=1,13
    res12 = res12+cheb12(i)*rg(i)
    res24 = res12+cheb24(i)*rg(i)
  end do
  do i=14,25
    res24 = res24+cheb24(i)*rg(i)
  end do
  go to 130
!
!  compute the chebyshev series expansion of the
!  following function
!  f4 = f1*log(0.5*(b+b-br-a)-0.5*(br-a)*x)
!
   70 fval(1) = fval(1)* log (fix-hlgth)
  fval(13) = fval(13)* log (fix)
  fval(25) = fval(25)* log (fix+hlgth)
  do i=2,12
    u = hlgth*x(i-1)
    isym = 26-i
    fval(i) = fval(i)* log (fix-u)
    fval(isym) = fval(isym)* log (fix+u)
  end do
  call dqcheb(x,fval,cheb12,cheb24)
!
!  integr = 3  (or 4)
!
  do i=1,13
    res12 = res12+cheb12(i)*ri(i)
    res24 = res24+cheb24(i)*ri(i)
  end do

  do i=14,25
    res24 = res24+cheb24(i)*ri(i)
  end do
  if(integr.eq.3) go to 130
!
!  integr = 4
!
  dc = log (br-bl)
  result = res24*dc
  abserr =  abs ( (res24-res12)*dc)
  res12 = 0.0D+00
  res24 = 0.0D+00
  do i=1,13
    res12 = res12+cheb12(i)*rg(i)
    res24 = res24+cheb24(i)*rg(i)
  end do
  do i=14,25
    res24 = res24+cheb24(i)*rg(i)
  end do
  130 result = (result+res24)*factor
  abserr = (abserr+ abs ( res24-res12))*factor
  go to 270
!
!  this part of the program is executed only if b = br.
!
!  compute the chebyshev series expansion of the following function:
!
!    f2 = (0.5*(b+bl-a-a)+0.5*(b-bl)*x)**alfa*f(0.5*(b-bl)*x+0.5*(b+bl))
!
  140 hlgth = 0.5D+00*(br-bl)
  centr = 0.5D+00*(br+bl)
  fix = centr-a
  fval(1) = 0.5D+00*f(hlgth+centr)*(fix+hlgth)**alfa
  fval(13) = f(centr)*(fix**alfa)
  fval(25) = 0.5D+00*f(centr-hlgth)*(fix-hlgth)**alfa
  do i=2,12
    u = hlgth*x(i-1)
    isym = 26-i
    fval(i) = f(u+centr)*(fix+u)**alfa
    fval(isym) = f(centr-u)*(fix-u)**alfa
  end do
  factor = hlgth**(beta+0.1D+01)
  result = 0.0D+00
  abserr = 0.0D+00
  res12 = 0.0D+00
  res24 = 0.0D+00
  if(integr.eq.2.or.integr.eq.4) go to 200
!
!  integr = 1  (or 3)
!
  call dqcheb(x,fval,cheb12,cheb24)

  do i=1,13
    res12 = res12+cheb12(i)*rj(i)
    res24 = res24+cheb24(i)*rj(i)
  end do

  do i=14,25
    res24 = res24+cheb24(i)*rj(i)
  end do

  if(integr.eq.1) go to 260
!
! integr = 3
!
  dc = log (br-bl)
  result = res24*dc
  abserr =  abs ( (res24-res12)*dc)
  res12 = 0.0D+00
  res24 = 0.0D+00
  do i=1,13
    res12 = res12+cheb12(i)*rh(i)
    res24 = res24+cheb24(i)*rh(i)
  end do

  do i=14,25
    res24 = res24+cheb24(i)*rh(i)
  end do
  go to 260
!
!  compute the chebyshev series expansion of the
!  following function
!  f3 = f2*log(0.5*(b-bl)*x+0.5*(b+bl-a-a))
!
  200 fval(1) = fval(1)* log (hlgth+fix)
  fval(13) = fval(13)* log (fix)
  fval(25) = fval(25)* log (fix-hlgth)
  do i=2,12
    u = hlgth*x(i-1)
    isym = 26-i
    fval(i) = fval(i)* log (u+fix)
    fval(isym) = fval(isym)* log (fix-u)
  end do
  call dqcheb(x,fval,cheb12,cheb24)
!
!  integr = 2  (or 4)
!
  do i=1,13
    res12 = res12+cheb12(i)*rj(i)
    res24 = res24+cheb24(i)*rj(i)
  end do

  do i=14,25
    res24 = res24+cheb24(i)*rj(i)
  end do

  if(integr.eq.2) go to 260
  dc = log (br-bl)
  result = res24*dc
  abserr =  abs ( (res24-res12)*dc)
  res12 = 0.0D+00
  res24 = 0.0D+00
!
!  integr = 4
!
  do i=1,13
    res12 = res12+cheb12(i)*rh(i)
    res24 = res24+cheb24(i)*rh(i)
  end do

  do i=14,25
    res24 = res24+cheb24(i)*rh(i)
  end do

  260 result = (result+res24)*factor
  abserr = (abserr+ abs ( res24-res12))*factor
  270 return
end

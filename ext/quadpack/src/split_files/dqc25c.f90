subroutine dqc25c(f,a,b,c,result,abserr,krul,neval)

!*****************************************************************************80
!
!! DQC25C returns integration rules for Cauchy Principal Value integrals.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  to compute i = integral of f*w over (a,b) with
!      error estimate, where w(x) = 1/(x-c)
!
!  Parameters:
!
!     f      - real ( kind = 8 )
!              function subprogram defining the integrand function
!              f(x). the actual name for f needs to be declared
!              e x t e r n a l  in the driver program.
!
!     a      - real ( kind = 8 )
!              left end point of the integration interval
!
!     b      - real ( kind = 8 )
!              right end point of the integration interval, b.gt.a
!
!     c      - real ( kind = 8 )
!              parameter in the weight function
!
!     result - real ( kind = 8 )
!              approximation to the integral
!              result is computed by using a generalized
!              clenshaw-curtis method if c lies within ten percent
!              of the integration interval. in the other case the
!              15-point kronrod rule obtained by optimal addition
!              of abscissae to the 7-point gauss rule, is applied.
!
!     abserr - real ( kind = 8 )
!              estimate of the modulus of the absolute error,
!              which should equal or exceed abs(i-result)
!
!     krul   - integer ( kind = 4 )
!              key which is decreased by 1 if the 15-point
!              gauss-kronrod scheme has been used
!
!     neval  - integer ( kind = 4 )
!              number of integrand evaluations
!
!  Local Parameters:
!
!     fval   - value of the function f at the points
!              cos(k*pi/24),  k = 0, ..., 24
!     cheb12 - chebyshev series expansion coefficients,
!              for the function f, of degree 12
!     cheb24 - chebyshev series expansion coefficients,
!              for the function f, of degree 24
!     res12  - approximation to the integral corresponding
!              to the use of cheb12
!     res24  - approximation to the integral corresponding
!              to the use of cheb24
!     dqwgtc - external function subprogram defining
!              the weight function
!     hlgth  - half-length of the interval
!     centr  - mid point of the interval
!
!     the vector x contains the values cos(k*pi/24),
!     k = 1, ..., 11, to be used for the chebyshev series
!     expansion of f
!
  implicit none

  real ( kind = 8 ) a,abserr,ak22,amom0,amom1,amom2,b,c,cc,centr, &
    cheb12,cheb24,dqwgtc,f,fval,hlgth,p2,p3,p4,resabs, &
    resasc,result,res12,res24,u,x
  integer ( kind = 4 ) i,isym,k,kp,krul,neval
  dimension x(11),fval(25),cheb12(13),cheb24(25)

  external f
  external dqwgtc

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
!
!  check the position of c.
!
  cc = (0.2D+01*c-b-a)/(b-a)
  if( abs ( cc).lt.0.11D+01) go to 10
!
!  apply the 15-point gauss-kronrod scheme.
!
  krul = krul-1
  call dqk15w(f,dqwgtc,c,p2,p3,p4,kp,a,b,result,abserr, &
    resabs,resasc)
  neval = 15
  if (resasc.eq.abserr) krul = krul+1
  go to 50
!
!  use the generalized clenshaw-curtis method.
!
   10 hlgth = 0.5D+00*(b-a)
  centr = 0.5D+00*(b+a)
  neval = 25
  fval(1) = 0.5D+00*f(hlgth+centr)
  fval(13) = f(centr)
  fval(25) = 0.5D+00*f(centr-hlgth)

  do i=2,12
    u = hlgth*x(i-1)
    isym = 26-i
    fval(i) = f(u+centr)
    fval(isym) = f(centr-u)
  end do
!
!  compute the chebyshev series expansion.
!
  call dqcheb(x,fval,cheb12,cheb24)
!
!  the modified chebyshev moments are computed by forward
!  recursion, using amom0 and amom1 as starting values.
!
  amom0 = log ( abs ( (0.1D+01-cc)/(0.1D+01+cc)))
  amom1 = 0.2D+01+cc*amom0
  res12 = cheb12(1)*amom0+cheb12(2)*amom1
  res24 = cheb24(1)*amom0+cheb24(2)*amom1

  do k=3,13
    amom2 = 0.2D+01*cc*amom1-amom0
    ak22 = (k-2)*(k-2)
    if((k/2)*2.eq.k) amom2 = amom2-0.4D+01/(ak22-0.1D+01)
    res12 = res12+cheb12(k)*amom2
    res24 = res24+cheb24(k)*amom2
    amom0 = amom1
    amom1 = amom2
  end do

  do k=14,25
    amom2 = 0.2D+01*cc*amom1-amom0
    ak22 = (k-2)*(k-2)
    if((k/2)*2.eq.k) amom2 = amom2-0.4D+01/(ak22-0.1D+01)
    res24 = res24+cheb24(k)*amom2
    amom0 = amom1
    amom1 = amom2
  end do

  result = res24
  abserr =  abs ( res24-res12)
   50 continue

  return
end

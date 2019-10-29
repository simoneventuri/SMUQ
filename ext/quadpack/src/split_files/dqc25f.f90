subroutine dqc25f(f,a,b,omega,integr,nrmom,maxp1,ksave,result, &
     abserr,neval,resabs,resasc,momcom,chebmo)

!*****************************************************************************80
!
!! DQC25F returns integration rules for functions with a COS or SIN factor.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  to compute the integral i=integral of f(x) over (a,b)
!      where w(x) = cos(omega*x) or w(x)=sin(omega*x) and to
!      compute j = integral of abs(f) over (a,b). for small value
!      of omega or small intervals (a,b) the 15-point gauss-kronro
!      rule is used. otherwise a generalized clenshaw-curtis
!      method is used.
!
!  Parameters:
!
!   on entry
!     f      - real ( kind = 8 )
!              function subprogram defining the integrand
!              function f(x). the actual name for f needs to
!              be declared e x t e r n a l in the calling program.
!
!     a      - real ( kind = 8 )
!              lower limit of integration
!
!     b      - real ( kind = 8 )
!              upper limit of integration
!
!     omega  - real ( kind = 8 )
!              parameter in the weight function
!
!     integr - integer ( kind = 4 )
!              indicates which weight function is to be used
!                 integr = 1   w(x) = cos(omega*x)
!                 integr = 2   w(x) = sin(omega*x)
!
!     nrmom  - integer ( kind = 4 )
!              the length of interval (a,b) is equal to the length
!              of the original integration interval divided by
!              2**nrmom (we suppose that the routine is used in an
!              adaptive integration process, otherwise set
!              nrmom = 0). nrmom must be zero at the first call.
!
!     maxp1  - integer ( kind = 4 )
!              gives an upper bound on the number of chebyshev
!              moments which can be stored, i.e. for the
!              intervals of lengths abs(bb-aa)*2**(-l),
!              l = 0,1,2, ..., maxp1-2.
!
!     ksave  - integer ( kind = 4 )
!              key which is one when the moments for the
!              current interval have been computed
!
!   on return
!     result - real ( kind = 8 )
!              approximation to the integral i
!
!     abserr - real ( kind = 8 )
!              estimate of the modulus of the absolute
!              error, which should equal or exceed abs(i-result)
!
!     neval  - integer ( kind = 4 )
!              number of integrand evaluations
!
!     resabs - real ( kind = 8 )
!              approximation to the integral j
!
!     resasc - real ( kind = 8 )
!              approximation to the integral of abs(f-i/(b-a))
!
!   on entry and return
!     momcom - integer ( kind = 4 )
!              for each interval length we need to compute the
!              chebyshev moments. momcom counts the number of
!              intervals for which these moments have already been
!              computed. if nrmom.lt.momcom or ksave = 1, the
!              chebyshev moments for the interval (a,b) have
!              already been computed and stored, otherwise we
!              compute them and we increase momcom.
!
!     chebmo - real ( kind = 8 )
!              array of dimension at least (maxp1,25) containing
!              the modified chebyshev moments for the first momcom
!              momcom interval lengths
!
!  Local Parameters:
!
!    the vector x contains the values cos(k*pi/24)
!    k = 1, ...,11, to be used for the chebyshev expansion of f
!
!     centr  - mid point of the integration interval
!     hlgth  - half-length of the integration interval
!     fval   - value of the function f at the points
!              (b-a)*0.5*cos(k*pi/12) + (b+a)*0.5, k = 0, ..., 24
!     cheb12 - coefficients of the chebyshev series expansion
!              of degree 12, for the function f, in the
!              interval (a,b)
!     cheb24 - coefficients of the chebyshev series expansion
!              of degree 24, for the function f, in the
!              interval (a,b)
!     resc12 - approximation to the integral of
!              cos(0.5*(b-a)*omega*x)*f(0.5*(b-a)*x+0.5*(b+a))
!              over (-1,+1), using the chebyshev series
!              expansion of degree 12
!     resc24 - approximation to the same integral, using the
!              chebyshev series expansion of degree 24
!     ress12 - the analogue of resc12 for the sine
!     ress24 - the analogue of resc24 for the sine
!
!
!     machine dependent constant
!
!     oflow is the largest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,abserr,ac,an,an2,as,asap,ass,b,centr,chebmo, &
    cheb12,cheb24,conc,cons,cospar,d,dqwgtf,d1, &
    d2,estc,ests,f,fval,hlgth,oflow,omega,parint,par2,par22, &
    p2,p3,p4,resabs,resasc,resc12,resc24,ress12,ress24,result, &
    sinpar,v,x
  integer ( kind = 4 ) i,iers,integr,isym,j,k,ksave,m,momcom,neval, maxp1,&
    noequ,noeq1,nrmom
  dimension chebmo(maxp1,25),cheb12(13),cheb24(25),d(25),d1(25), &
    d2(25),fval(25),v(28),x(11)

  external f,dqwgtf

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

  oflow = huge ( oflow )
  centr = 0.5D+00*(b+a)
  hlgth = 0.5D+00*(b-a)
  parint = omega*hlgth
!
!  compute the integral using the 15-point gauss-kronrod
!  formula if the value of the parameter in the integrand is small.
!
  if( abs ( parint).gt.0.2D+01) go to 10
  call dqk15w(f,dqwgtf,omega,p2,p3,p4,integr,a,b,result, &
    abserr,resabs,resasc)
  neval = 15
  go to 170
!
!  compute the integral using the generalized clenshaw-
!  curtis method.
!
   10 conc = hlgth*dcos(centr*omega)
  cons = hlgth*dsin(centr*omega)
  resasc = oflow
  neval = 25
!
!  check whether the chebyshev moments for this interval
!  have already been computed.
!
  if(nrmom.lt.momcom.or.ksave.eq.1) go to 120
!
!  compute a new set of chebyshev moments.
!
  m = momcom+1
  par2 = parint*parint
  par22 = par2+0.2D+01
  sinpar = dsin(parint)
  cospar = dcos(parint)
!
!  compute the chebyshev moments with respect to cosine.
!
  v(1) = 0.2D+01*sinpar/parint
  v(2) = (0.8D+01*cospar+(par2+par2-0.8D+01)*sinpar/parint)/par2
  v(3) = (0.32D+02*(par2-0.12D+02)*cospar+(0.2D+01* &
    ((par2-0.80D+02)*par2+0.192D+03)*sinpar)/parint)/(par2*par2)
  ac = 0.8D+01*cospar
  as = 0.24D+02*parint*sinpar
  if( abs ( parint).gt.0.24D+02) go to 30
!
!  compute the chebyshev moments as the solutions of a
!  boundary value problem with 1 initial value (v(3)) and 1
!  end value (computed using an asymptotic formula).
!
  noequ = 25
  noeq1 = noequ-1
  an = 0.6D+01

  do k = 1,noeq1
    an2 = an*an
    d(k) = -0.2D+01*(an2-0.4D+01)*(par22-an2-an2)
    d2(k) = (an-0.1D+01)*(an-0.2D+01)*par2
    d1(k+1) = (an+0.3D+01)*(an+0.4D+01)*par2
    v(k+3) = as-(an2-0.4D+01)*ac
    an = an+0.2D+01
  end do

  an2 = an*an
  d(noequ) = -0.2D+01*(an2-0.4D+01)*(par22-an2-an2)
  v(noequ+3) = as-(an2-0.4D+01)*ac
  v(4) = v(4)-0.56D+02*par2*v(3)
  ass = parint*sinpar
  asap = (((((0.210D+03*par2-0.1D+01)*cospar-(0.105D+03*par2 &
    -0.63D+02)*ass)/an2-(0.1D+01-0.15D+02*par2)*cospar &
    +0.15D+02*ass)/an2-cospar+0.3D+01*ass)/an2-cospar)/an2
  v(noequ+3) = v(noequ+3)-0.2D+01*asap*par2*(an-0.1D+01)* &
     (an-0.2D+01)
!
!  solve the tridiagonal system by means of gaussian
!  elimination with partial pivoting.
!
  call dgtsl(noequ,d1,d,d2,v(4),iers)
  go to 50
!
!  compute the chebyshev moments by means of forward recursion.
!
   30 an = 0.4D+01

  do i = 4,13
    an2 = an*an
    v(i) = ((an2-0.4D+01)*(0.2D+01*(par22-an2-an2)*v(i-1)-ac) &
    +as-par2*(an+0.1D+01)*(an+0.2D+01)*v(i-2))/ &
    (par2*(an-0.1D+01)*(an-0.2D+01))
    an = an+0.2D+01
  end do

   50 continue

  do j = 1,13
    chebmo(m,2*j-1) = v(j)
  end do
!
!  compute the chebyshev moments with respect to sine.
!
  v(1) = 0.2D+01*(sinpar-parint*cospar)/par2
  v(2) = (0.18D+02-0.48D+02/par2)*sinpar/par2 &
    +(-0.2D+01+0.48D+02/par2)*cospar/parint
  ac = -0.24D+02*parint*cospar
  as = -0.8D+01*sinpar
  if( abs ( parint).gt.0.24D+02) go to 80
!
!  compute the chebyshev moments as the solutions of a boundary
!  value problem with 1 initial value (v(2)) and 1 end value
!  (computed using an asymptotic formula).
!
  an = 0.5D+01

  do k = 1,noeq1
    an2 = an*an
    d(k) = -0.2D+01*(an2-0.4D+01)*(par22-an2-an2)
    d2(k) = (an-0.1D+01)*(an-0.2D+01)*par2
    d1(k+1) = (an+0.3D+01)*(an+0.4D+01)*par2
    v(k+2) = ac+(an2-0.4D+01)*as
    an = an+0.2D+01
  end do

  an2 = an*an
  d(noequ) = -0.2D+01*(an2-0.4D+01)*(par22-an2-an2)
  v(noequ+2) = ac+(an2-0.4D+01)*as
  v(3) = v(3)-0.42D+02*par2*v(2)
  ass = parint*cospar
  asap = (((((0.105D+03*par2-0.63D+02)*ass+(0.210D+03*par2 &
    -0.1D+01)*sinpar)/an2+(0.15D+02*par2-0.1D+01)*sinpar- &
    0.15D+02*ass)/an2-0.3D+01*ass-sinpar)/an2-sinpar)/an2
  v(noequ+2) = v(noequ+2)-0.2D+01*asap*par2*(an-0.1D+01) &
    *(an-0.2D+01)
!
!  solve the tridiagonal system by means of gaussian
!  elimination with partial pivoting.
!
  call dgtsl(noequ,d1,d,d2,v(3),iers)
  go to 100
!
!  compute the chebyshev moments by means of forward recursion.
!
   80 an = 0.3D+01

  do i = 3,12
    an2 = an*an
    v(i) = ((an2-0.4D+01)*(0.2D+01*(par22-an2-an2)*v(i-1)+as) &
    +ac-par2*(an+0.1D+01)*(an+0.2D+01)*v(i-2)) &
    /(par2*(an-0.1D+01)*(an-0.2D+01))
    an = an+0.2D+01
  end do

  100 continue

  do j = 1,12
    chebmo(m,2*j) = v(j)
  end do

  120 if (nrmom.lt.momcom) m = nrmom+1
   if (momcom.lt.(maxp1-1).and.nrmom.ge.momcom) momcom = momcom+1
!
!  compute the coefficients of the chebyshev expansions
!  of degrees 12 and 24 of the function f.
!
  fval(1) = 0.5D+00*f(centr+hlgth)
  fval(13) = f(centr)
  fval(25) = 0.5D+00*f(centr-hlgth)
  do i = 2,12
    isym = 26-i
    fval(i) = f(hlgth*x(i-1)+centr)
    fval(isym) = f(centr-hlgth*x(i-1))
  end do
  call dqcheb(x,fval,cheb12,cheb24)
!
!  compute the integral and error estimates.
!
  resc12 = cheb12(13)*chebmo(m,13)
  ress12 = 0.0D+00
  k = 11
  do j = 1,6
    resc12 = resc12+cheb12(k)*chebmo(m,k)
    ress12 = ress12+cheb12(k+1)*chebmo(m,k+1)
    k = k-2
  end do
  resc24 = cheb24(25)*chebmo(m,25)
  ress24 = 0.0D+00
  resabs =  abs ( cheb24(25))
  k = 23
  do j = 1,12
    resc24 = resc24+cheb24(k)*chebmo(m,k)
    ress24 = ress24+cheb24(k+1)*chebmo(m,k+1)
    resabs =  abs ( cheb24(k))+ abs ( cheb24(k+1))
    k = k-2
  end do
  estc =  abs ( resc24-resc12)
  ests =  abs ( ress24-ress12)
  resabs = resabs* abs ( hlgth)
  if(integr.eq.2) go to 160
  result = conc*resc24-cons*ress24
  abserr =  abs ( conc*estc)+ abs ( cons*ests)
  go to 170
  160 result = conc*ress24+cons*resc24
  abserr =  abs ( conc*ests)+ abs ( cons*estc)
  170 continue

  return
end

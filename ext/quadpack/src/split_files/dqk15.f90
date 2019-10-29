subroutine dqk15(f,a,b,result,abserr,resabs,resasc)

!*****************************************************************************80
!
!! DQK15 carries out a 15 point Gauss-Kronrod quadrature rule.
!
!     the abscissae and weights are given for the interval (-1,1).
!     because of symmetry only the positive abscissae and their
!     corresponding weights are given.
!
!     xgk    - abscissae of the 15-point kronrod rule
!              xgk(2), xgk(4), ...  abscissae of the 7-point
!              gauss rule
!              xgk(1), xgk(3), ...  abscissae which are optimally
!              added to the 7-point gauss rule
!
!     wgk    - weights of the 15-point kronrod rule
!
!     wg     - weights of the 7-point gauss rule
!
!
!   gauss quadrature weights and kronron quadrature abscissae and weights
!   as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
!   bell labs, nov. 1981.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  to compute i = integral of f over (a,b), with error
!                     estimate
!                 j = integral of abs(f) over (a,b)
!  Parameters:
!
!      on entry
!        f      - real ( kind = 8 )
!                 function subprogram defining the integrand
!                 function f(x). the actual name for f needs to be
!                 declared e x t e r n a l in the calling program.
!
!        a      - real ( kind = 8 )
!                 lower limit of integration
!
!        b      - real ( kind = 8 )
!                 upper limit of integration
!
!      on return
!        result - real ( kind = 8 )
!                 approximation to the integral i
!                 result is computed by applying the 15-point
!                 kronrod rule (resk) obtained by optimal addition
!                 of abscissae to the7-point gauss rule(resg).
!
!        abserr - real ( kind = 8 )
!                 estimate of the modulus of the absolute error,
!                 which should not exceed abs(i-result)
!
!        resabs - real ( kind = 8 )
!                 approximation to the integral j
!
!        resasc - real ( kind = 8 )
!                 approximation to the integral of abs(f-i/(b-a))
!                 over (a,b)
!
!  Local Parameters:
!
!     centr  - mid point of the interval
!     hlgth  - half-length of the interval
!     absc   - abscissa
!     fval*  - function value
!     resg   - result of the 7-point gauss formula
!     resk   - result of the 15-point kronrod formula
!     reskh  - approximation to the mean value of f over (a,b),
!              i.e. to i/(b-a)
!
!     machine dependent constants
!
!     epmach is the largest relative spacing.
!     uflow is the smallest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,absc,abserr,b,centr,dhlgth, &
    epmach,f,fc,fsum,fval1,fval2,fv1,fv2,hlgth,resabs,resasc, &
    resg,resk,reskh,result,uflow,wg,wgk,xgk
  integer ( kind = 4 ) j,jtw,jtwm1
  external f
  dimension fv1(7),fv2(7),wg(4),wgk(8),xgk(8)

  data wg  (  1) / 0.129484966168869693270611432679082d0 /
  data wg  (  2) / 0.279705391489276667901467771423780d0 /
  data wg  (  3) / 0.381830050505118944950369775488975d0 /
  data wg  (  4) / 0.417959183673469387755102040816327d0 /

  data xgk (  1) / 0.991455371120812639206854697526329d0 /
  data xgk (  2) / 0.949107912342758524526189684047851d0 /
  data xgk (  3) / 0.864864423359769072789712788640926d0 /
  data xgk (  4) / 0.741531185599394439863864773280788d0 /
  data xgk (  5) / 0.586087235467691130294144838258730d0 /
  data xgk (  6) / 0.405845151377397166906606412076961d0 /
  data xgk (  7) / 0.207784955007898467600689403773245d0 /
  data xgk (  8) / 0.000000000000000000000000000000000d0 /

  data wgk (  1) / 0.022935322010529224963732008058970d0 /
  data wgk (  2) / 0.063092092629978553290700663189204d0 /
  data wgk (  3) / 0.104790010322250183839876322541518d0 /
  data wgk (  4) / 0.140653259715525918745189590510238d0 /
  data wgk (  5) / 0.169004726639267902826583426598550d0 /
  data wgk (  6) / 0.190350578064785409913256402421014d0 /
  data wgk (  7) / 0.204432940075298892414161999234649d0 /
  data wgk (  8) / 0.209482141084727828012999174891714d0 /

  epmach = epsilon ( epmach )
  uflow = tiny ( uflow )
  centr = 0.5D+00*(a+b)
  hlgth = 0.5D+00*(b-a)
  dhlgth =  abs ( hlgth)
!
!  compute the 15-point kronrod approximation to
!  the integral, and estimate the absolute error.
!
  fc = f(centr)
  resg = fc*wg(4)
  resk = fc*wgk(8)
  resabs =  abs ( resk)

  do j=1,3
    jtw = j*2
    absc = hlgth*xgk(jtw)
    fval1 = f(centr-absc)
    fval2 = f(centr+absc)
    fv1(jtw) = fval1
    fv2(jtw) = fval2
    fsum = fval1+fval2
    resg = resg+wg(j)*fsum
    resk = resk+wgk(jtw)*fsum
    resabs = resabs+wgk(jtw)*( abs ( fval1)+ abs ( fval2))
  end do

  do j = 1,4
    jtwm1 = j*2-1
    absc = hlgth*xgk(jtwm1)
    fval1 = f(centr-absc)
    fval2 = f(centr+absc)
    fv1(jtwm1) = fval1
    fv2(jtwm1) = fval2
    fsum = fval1+fval2
    resk = resk+wgk(jtwm1)*fsum
    resabs = resabs+wgk(jtwm1)*( abs ( fval1)+ abs ( fval2))
  end do

  reskh = resk*0.5D+00
  resasc = wgk(8)* abs ( fc-reskh)
  do j=1,7
    resasc = resasc+wgk(j)*( abs ( fv1(j)-reskh)+ abs ( fv2(j)-reskh))
  end do

  result = resk*hlgth
  resabs = resabs*dhlgth
  resasc = resasc*dhlgth
  abserr =  abs ( (resk-resg)*hlgth)
  if(resasc.ne.0.0D+00.and.abserr.ne.0.0D+00) &
    abserr = resasc* min (0.1D+01,(0.2D+03*abserr/resasc)**1.5D+00)
  if(resabs.gt.uflow/(0.5D+02*epmach)) abserr = max &
    ((epmach*0.5D+02)*resabs,abserr)

  return
end

subroutine dqk21(f,a,b,result,abserr,resabs,resasc)

!*****************************************************************************80
!
!! DQK21 carries out a 21 point Gauss-Kronrod quadrature rule.
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
!
!  Parameters:
!
!      on entry
!        f      - real ( kind = 8 )
!                 function subprogram defining the integrand
!                 function f(x). the actual name for f needs to be
!                 declared e x t e r n a l in the driver program.
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
!                 result is computed by applying the 21-point
!                 kronrod rule (resk) obtained by optimal addition
!                 of abscissae to the 10-point gauss rule (resg).
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
!
!     the abscissae and weights are given for the interval (-1,1).
!     because of symmetry only the positive abscissae and their
!     corresponding weights are given.
!
!     xgk    - abscissae of the 21-point kronrod rule
!              xgk(2), xgk(4), ...  abscissae of the 10-point
!              gauss rule
!              xgk(1), xgk(3), ...  abscissae which are optimally
!              added to the 10-point gauss rule
!
!     wgk    - weights of the 21-point kronrod rule
!
!     wg     - weights of the 10-point gauss rule
!
!
! gauss quadrature weights and kronron quadrature abscissae and weights
! as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
! bell labs, nov. 1981.
!
!     centr  - mid point of the interval
!     hlgth  - half-length of the interval
!     absc   - abscissa
!     fval*  - function value
!     resg   - result of the 10-point gauss formula
!     resk   - result of the 21-point kronrod formula
!     reskh  - approximation to the mean value of f over (a,b),
!              i.e. to i/(b-a)
!
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
  dimension fv1(10),fv2(10),wg(5),wgk(11),xgk(11)

  data wg  (  1) / 0.066671344308688137593568809893332d0 /
  data wg  (  2) / 0.149451349150580593145776339657697d0 /
  data wg  (  3) / 0.219086362515982043995534934228163d0 /
  data wg  (  4) / 0.269266719309996355091226921569469d0 /
  data wg  (  5) / 0.295524224714752870173892994651338d0 /

  data xgk (  1) / 0.995657163025808080735527280689003d0 /
  data xgk (  2) / 0.973906528517171720077964012084452d0 /
  data xgk (  3) / 0.930157491355708226001207180059508d0 /
  data xgk (  4) / 0.865063366688984510732096688423493d0 /
  data xgk (  5) / 0.780817726586416897063717578345042d0 /
  data xgk (  6) / 0.679409568299024406234327365114874d0 /
  data xgk (  7) / 0.562757134668604683339000099272694d0 /
  data xgk (  8) / 0.433395394129247190799265943165784d0 /
  data xgk (  9) / 0.294392862701460198131126603103866d0 /
  data xgk ( 10) / 0.148874338981631210884826001129720d0 /
  data xgk ( 11) / 0.000000000000000000000000000000000d0 /

  data wgk (  1) / 0.011694638867371874278064396062192d0 /
  data wgk (  2) / 0.032558162307964727478818972459390d0 /
  data wgk (  3) / 0.054755896574351996031381300244580d0 /
  data wgk (  4) / 0.075039674810919952767043140916190d0 /
  data wgk (  5) / 0.093125454583697605535065465083366d0 /
  data wgk (  6) / 0.109387158802297641899210590325805d0 /
  data wgk (  7) / 0.123491976262065851077958109831074d0 /
  data wgk (  8) / 0.134709217311473325928054001771707d0 /
  data wgk (  9) / 0.142775938577060080797094273138717d0 /
  data wgk ( 10) / 0.147739104901338491374841515972068d0 /
  data wgk ( 11) / 0.149445554002916905664936468389821d0 /

  epmach = epsilon ( epmach )
  uflow = tiny ( uflow )
  centr = 0.5D+00*(a+b)
  hlgth = 0.5D+00*(b-a)
  dhlgth =  abs ( hlgth)
!
!  compute the 21-point kronrod approximation to
!  the integral, and estimate the absolute error.
!
  resg = 0.0D+00
  fc = f(centr)
  resk = wgk(11)*fc
  resabs =  abs ( resk)
  do j=1,5
    jtw = 2*j
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

  do j = 1,5
    jtwm1 = 2*j-1
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
  resasc = wgk(11)* abs ( fc-reskh)

  do j=1,10
    resasc = resasc+wgk(j)*( abs ( fv1(j)-reskh)+ abs ( fv2(j)-reskh))
  end do

  result = resk*hlgth
  resabs = resabs*dhlgth
  resasc = resasc*dhlgth
  abserr =  abs ( (resk-resg)*hlgth)
  if(resasc.ne.0.0D+00.and.abserr.ne.0.0D+00) &
    abserr = resasc*min(0.1D+01,(0.2D+03*abserr/resasc)**1.5D+00)
  if(resabs.gt.uflow/(0.5D+02*epmach)) abserr = max &
    ((epmach*0.5D+02)*resabs,abserr)

  return
end

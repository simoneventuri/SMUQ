subroutine dqk15w(f,w,p1,p2,p3,p4,kp,a,b,result,abserr, resabs,resasc)

!*****************************************************************************80
!
!! DQK15W applies a 15 point Gauss-Kronrod rule for a weighted integrand.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  to compute i = integral of f*w over (a,b), with error
!                     estimate
!                 j = integral of abs(f*w) over (a,b)
!
!  Parameters:
!
!       on entry
!        f      - real ( kind = 8 )
!                 function subprogram defining the integrand
!                 function f(x). the actual name for f needs to be
!                 declared e x t e r n a l in the driver program.
!
!        w      - real ( kind = 8 )
!                 function subprogram defining the integrand
!                 weight function w(x). the actual name for w
!                 needs to be declared e x t e r n a l in the
!                 calling program.
!
!        p1, p2, p3, p4 - real ( kind = 8 )
!                 parameters in the weight function
!
!        kp     - integer ( kind = 4 )
!                 key for indicating the type of weight function
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
!                 of abscissae to the 7-point gauss rule (resg).
!
!        abserr - real ( kind = 8 )
!                 estimate of the modulus of the absolute error,
!                 which should equal or exceed abs(i-result)
!
!        resabs - real ( kind = 8 )
!                 approximation to the integral of abs(f)
!
!        resasc - real ( kind = 8 )
!                 approximation to the integral of abs(f-i/(b-a))
!
!  Local Parameters:
!
!     the abscissae and weights are given for the interval (-1,1).
!     because of symmetry only the positive abscissae and their
!     corresponding weights are given.
!
!     xgk    - abscissae of the 15-point gauss-kronrod rule
!              xgk(2), xgk(4), ... abscissae of the 7-point
!              gauss rule
!              xgk(1), xgk(3), ... abscissae which are optimally
!              added to the 7-point gauss rule
!
!     wgk    - weights of the 15-point gauss-kronrod rule
!
!     wg     - weights of the 7-point gauss rule
!
!     centr  - mid point of the interval
!     hlgth  - half-length of the interval
!     absc*  - abscissa
!     fval*  - function value
!     resg   - result of the 7-point gauss formula
!     resk   - result of the 15-point kronrod formula
!     reskh  - approximation to the mean value of f*w over (a,b),
!              i.e. to i/(b-a)
!
!     machine dependent constants
!
!     epmach is the largest relative spacing.
!     uflow is the smallest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,absc,absc1,absc2,abserr,b,centr,dhlgth, &
    epmach,f,fc,fsum,fval1,fval2,fv1,fv2,hlgth, &
    p1,p2,p3,p4,resabs,resasc,resg,resk,reskh,result,uflow,w,wg,wgk, &
    xgk
  integer ( kind = 4 ) j,jtw,jtwm1,kp
  external f,w

  dimension fv1(7),fv2(7),xgk(8),wgk(8),wg(4)

  data xgk(1),xgk(2),xgk(3),xgk(4),xgk(5),xgk(6),xgk(7),xgk(8)/ &
       0.9914553711208126D+00,     0.9491079123427585D+00, &
       0.8648644233597691D+00,     0.7415311855993944D+00, &
       0.5860872354676911D+00,     0.4058451513773972D+00, &
       0.2077849550078985D+00,     0.0000000000000000D+00/

  data wgk(1),wgk(2),wgk(3),wgk(4),wgk(5),wgk(6),wgk(7),wgk(8)/ &
       0.2293532201052922D-01,     0.6309209262997855D-01, &
       0.1047900103222502D+00,     0.1406532597155259D+00, &
       0.1690047266392679D+00,     0.1903505780647854D+00, &
       0.2044329400752989D+00,     0.2094821410847278D+00/

  data wg(1),wg(2),wg(3),wg(4)/ &
       0.1294849661688697D+00,    0.2797053914892767D+00, &
       0.3818300505051889D+00,    0.4179591836734694D+00/

  epmach = epsilon ( epmach )
  uflow = tiny ( uflow )
  centr = 0.5D+00*(a+b)
  hlgth = 0.5D+00*(b-a)
  dhlgth =  abs ( hlgth)
!
!  compute the 15-point kronrod approximation to the
!  integral, and estimate the error.
!
  fc = f(centr)*w(centr,p1,p2,p3,p4,kp)
  resg = wg(4)*fc
  resk = wgk(8)*fc
  resabs =  abs ( resk)

  do j=1,3
    jtw = j*2
    absc = hlgth*xgk(jtw)
    absc1 = centr-absc
    absc2 = centr+absc
    fval1 = f(absc1)*w(absc1,p1,p2,p3,p4,kp)
    fval2 = f(absc2)*w(absc2,p1,p2,p3,p4,kp)
    fv1(jtw) = fval1
    fv2(jtw) = fval2
    fsum = fval1+fval2
    resg = resg+wg(j)*fsum
    resk = resk+wgk(jtw)*fsum
    resabs = resabs+wgk(jtw)*( abs ( fval1)+ abs ( fval2))
  end do

  do j=1,4
    jtwm1 = j*2-1
    absc = hlgth*xgk(jtwm1)
    absc1 = centr-absc
    absc2 = centr+absc
    fval1 = f(absc1)*w(absc1,p1,p2,p3,p4,kp)
    fval2 = f(absc2)*w(absc2,p1,p2,p3,p4,kp)
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
  if(resabs.gt.uflow/(0.5D+02*epmach)) abserr =  max ( (epmach* &
    0.5D+02)*resabs,abserr)

  return
end

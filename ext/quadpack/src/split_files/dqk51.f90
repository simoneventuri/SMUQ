subroutine dqk51(f,a,b,result,abserr,resabs,resasc)

!*****************************************************************************80
!
!! DQK51 carries out a 51 point Gauss-Kronrod quadrature rule.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  to compute i = integral of f over (a,b) with error
!                     estimate
!                 j = integral of abs(f) over (a,b)
!
!  Parameters:
!
!      on entry
!        f      - real ( kind = 8 )
!                 function defining the integrand
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
!                 result is computed by applying the 51-point
!                 kronrod rule (resk) obtained by optimal addition
!                 of abscissae to the 25-point gauss rule (resg).
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
!     the abscissae and weights are given for the interval (-1,1).
!     because of symmetry only the positive abscissae and their
!     corresponding weights are given.
!
!     xgk    - abscissae of the 51-point kronrod rule
!              xgk(2), xgk(4), ...  abscissae of the 25-point
!              gauss rule
!              xgk(1), xgk(3), ...  abscissae which are optimally
!              added to the 25-point gauss rule
!
!     wgk    - weights of the 51-point kronrod rule
!
!     wg     - weights of the 25-point gauss rule
!
! gauss quadrature weights and kronron quadrature abscissae and weights
! as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
! bell labs, nov. 1981.
!
!     centr  - mid point of the interval
!     hlgth  - half-length of the interval
!     absc   - abscissa
!     fval*  - function value
!     resg   - result of the 25-point gauss formula
!     resk   - result of the 51-point kronrod formula
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

  dimension fv1(25),fv2(25),xgk(26),wgk(26),wg(13)

  data wg  (  1) / 0.011393798501026287947902964113235d0 /
  data wg  (  2) / 0.026354986615032137261901815295299d0 /
  data wg  (  3) / 0.040939156701306312655623487711646d0 /
  data wg  (  4) / 0.054904695975835191925936891540473d0 /
  data wg  (  5) / 0.068038333812356917207187185656708d0 /
  data wg  (  6) / 0.080140700335001018013234959669111d0 /
  data wg  (  7) / 0.091028261982963649811497220702892d0 /
  data wg  (  8) / 0.100535949067050644202206890392686d0 /
  data wg  (  9) / 0.108519624474263653116093957050117d0 /
  data wg  ( 10) / 0.114858259145711648339325545869556d0 /
  data wg  ( 11) / 0.119455763535784772228178126512901d0 /
  data wg  ( 12) / 0.122242442990310041688959518945852d0 /
  data wg  ( 13) / 0.123176053726715451203902873079050d0 /

  data xgk (  1) / 0.999262104992609834193457486540341d0 /
  data xgk (  2) / 0.995556969790498097908784946893902d0 /
  data xgk (  3) / 0.988035794534077247637331014577406d0 /
  data xgk (  4) / 0.976663921459517511498315386479594d0 /
  data xgk (  5) / 0.961614986425842512418130033660167d0 /
  data xgk (  6) / 0.942974571228974339414011169658471d0 /
  data xgk (  7) / 0.920747115281701561746346084546331d0 /
  data xgk (  8) / 0.894991997878275368851042006782805d0 /
  data xgk (  9) / 0.865847065293275595448996969588340d0 /
  data xgk ( 10) / 0.833442628760834001421021108693570d0 /
  data xgk ( 11) / 0.797873797998500059410410904994307d0 /
  data xgk ( 12) / 0.759259263037357630577282865204361d0 /
  data xgk ( 13) / 0.717766406813084388186654079773298d0 /
  data xgk ( 14) / 0.673566368473468364485120633247622d0 /
  data xgk ( 15) / 0.626810099010317412788122681624518d0 /
  data xgk ( 16) / 0.577662930241222967723689841612654d0 /
  data xgk ( 17) / 0.526325284334719182599623778158010d0 /
  data xgk ( 18) / 0.473002731445714960522182115009192d0 /
  data xgk ( 19) / 0.417885382193037748851814394594572d0 /
  data xgk ( 20) / 0.361172305809387837735821730127641d0 /
  data xgk ( 21) / 0.303089538931107830167478909980339d0 /
  data xgk ( 22) / 0.243866883720988432045190362797452d0 /
  data xgk ( 23) / 0.183718939421048892015969888759528d0 /
  data xgk ( 24) / 0.122864692610710396387359818808037d0 /
  data xgk ( 25) / 0.061544483005685078886546392366797d0 /
  data xgk ( 26) / 0.000000000000000000000000000000000d0 /

  data wgk (  1) / 0.001987383892330315926507851882843d0 /
  data wgk (  2) / 0.005561932135356713758040236901066d0 /
  data wgk (  3) / 0.009473973386174151607207710523655d0 /
  data wgk (  4) / 0.013236229195571674813656405846976d0 /
  data wgk (  5) / 0.016847817709128298231516667536336d0 /
  data wgk (  6) / 0.020435371145882835456568292235939d0 /
  data wgk (  7) / 0.024009945606953216220092489164881d0 /
  data wgk (  8) / 0.027475317587851737802948455517811d0 /
  data wgk (  9) / 0.030792300167387488891109020215229d0 /
  data wgk ( 10) / 0.034002130274329337836748795229551d0 /
  data wgk ( 11) / 0.037116271483415543560330625367620d0 /
  data wgk ( 12) / 0.040083825504032382074839284467076d0 /
  data wgk ( 13) / 0.042872845020170049476895792439495d0 /
  data wgk ( 14) / 0.045502913049921788909870584752660d0 /
  data wgk ( 15) / 0.047982537138836713906392255756915d0 /
  data wgk ( 16) / 0.050277679080715671963325259433440d0 /
  data wgk ( 17) / 0.052362885806407475864366712137873d0 /
  data wgk ( 18) / 0.054251129888545490144543370459876d0 /
  data wgk ( 19) / 0.055950811220412317308240686382747d0 /
  data wgk ( 20) / 0.057437116361567832853582693939506d0 /
  data wgk ( 21) / 0.058689680022394207961974175856788d0 /
  data wgk ( 22) / 0.059720340324174059979099291932562d0 /
  data wgk ( 23) / 0.060539455376045862945360267517565d0 /
  data wgk ( 24) / 0.061128509717053048305859030416293d0 /
  data wgk ( 25) / 0.061471189871425316661544131965264d0 /
  data wgk ( 26) / 0.061580818067832935078759824240066d0 /

  epmach = epsilon ( epmach )
  uflow = tiny ( uflow )
  centr = 0.5D+00*(a+b)
  hlgth = 0.5D+00*(b-a)
  dhlgth =  abs ( hlgth)
!
!  compute the 51-point kronrod approximation to
!  the integral, and estimate the absolute error.
!
  fc = f(centr)
  resg = wg(13)*fc
  resk = wgk(26)*fc
  resabs =  abs ( resk)

  do j=1,12
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

  do j = 1,13
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
  resasc = wgk(26)* abs ( fc-reskh)

  do j=1,25
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

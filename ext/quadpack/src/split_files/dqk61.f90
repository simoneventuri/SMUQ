subroutine dqk61(f,a,b,result,abserr,resabs,resasc)

!*****************************************************************************80
!
!! DQK61 carries out a 61 point Gauss-Kronrod quadrature rule.
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
!                 j = integral of  abs ( f) over (a,b)
!
!  Parameters:
!
!   on entry
!     f      - real ( kind = 8 )
!              function subprogram defining the integrand
!              function f(x). the actual name for f needs to be
!              declared e x t e r n a l in the calling program.
!
!     a      - real ( kind = 8 )
!              lower limit of integration
!
!     b      - real ( kind = 8 )
!              upper limit of integration
!
!   on return
!     result - real ( kind = 8 )
!              approximation to the integral i
!              result is computed by applying the 61-point
!              kronrod rule (resk) obtained by optimal addition of
!              abscissae to the 30-point gauss rule (resg).
!
!     abserr - real ( kind = 8 )
!              estimate of the modulus of the absolute error,
!              which should equal or exceed  abs ( i-result)
!
!     resabs - real ( kind = 8 )
!              approximation to the integral j
!
!     resasc - real ( kind = 8 )
!              approximation to the integral of  abs ( f-i/(b-a))
!
!  Local Parameters:
!
!     the abscissae and weights are given for the
!     interval (-1,1). because of symmetry only the positive
!     abscissae and their corresponding weights are given.
!
!     xgk   - abscissae of the 61-point kronrod rule
!             xgk(2), xgk(4)  ... abscissae of the 30-point
!             gauss rule
!             xgk(1), xgk(3)  ... optimally added abscissae
!             to the 30-point gauss rule
!
!     wgk   - weights of the 61-point kronrod rule
!
!     wg    - weigths of the 30-point gauss rule
!
!
!   gauss quadrature weights and kronron quadrature abscissae and weights
!   as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
!   bell labs, nov. 1981.
!
!     centr  - mid point of the interval
!     hlgth  - half-length of the interval
!     dabsc  - abscissa
!     fval*  - function value
!     resg   - result of the 30-point gauss rule
!     resk   - result of the 61-point kronrod rule
!     reskh  - approximation to the mean value of f
!              over (a,b), i.e. to i/(b-a)
!
!     machine dependent constants
!
!     epmach is the largest relative spacing.
!     uflow is the smallest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,dabsc,abserr,b,centr,dhlgth, &
    epmach,f,fc,fsum,fval1,fval2,fv1,fv2,hlgth,resabs,resasc, &
    resg,resk,reskh,result,uflow,wg,wgk,xgk
  integer ( kind = 4 ) j,jtw,jtwm1
  external f

  dimension fv1(30),fv2(30),xgk(31),wgk(31),wg(15)

  data wg  (  1) / 0.007968192496166605615465883474674d0 /
  data wg  (  2) / 0.018466468311090959142302131912047d0 /
  data wg  (  3) / 0.028784707883323369349719179611292d0 /
  data wg  (  4) / 0.038799192569627049596801936446348d0 /
  data wg  (  5) / 0.048402672830594052902938140422808d0 /
  data wg  (  6) / 0.057493156217619066481721689402056d0 /
  data wg  (  7) / 0.065974229882180495128128515115962d0 /
  data wg  (  8) / 0.073755974737705206268243850022191d0 /
  data wg  (  9) / 0.080755895229420215354694938460530d0 /
  data wg  ( 10) / 0.086899787201082979802387530715126d0 /
  data wg  ( 11) / 0.092122522237786128717632707087619d0 /
  data wg  ( 12) / 0.096368737174644259639468626351810d0 /
  data wg  ( 13) / 0.099593420586795267062780282103569d0 /
  data wg  ( 14) / 0.101762389748405504596428952168554d0 /
  data wg  ( 15) / 0.102852652893558840341285636705415d0 /

  data xgk (  1) / 0.999484410050490637571325895705811d0 /
  data xgk (  2) / 0.996893484074649540271630050918695d0 /
  data xgk (  3) / 0.991630996870404594858628366109486d0 /
  data xgk (  4) / 0.983668123279747209970032581605663d0 /
  data xgk (  5) / 0.973116322501126268374693868423707d0 /
  data xgk (  6) / 0.960021864968307512216871025581798d0 /
  data xgk (  7) / 0.944374444748559979415831324037439d0 /
  data xgk (  8) / 0.926200047429274325879324277080474d0 /
  data xgk (  9) / 0.905573307699907798546522558925958d0 /
  data xgk ( 10) / 0.882560535792052681543116462530226d0 /
  data xgk ( 11) / 0.857205233546061098958658510658944d0 /
  data xgk ( 12) / 0.829565762382768397442898119732502d0 /
  data xgk ( 13) / 0.799727835821839083013668942322683d0 /
  data xgk ( 14) / 0.767777432104826194917977340974503d0 /
  data xgk ( 15) / 0.733790062453226804726171131369528d0 /
  data xgk ( 16) / 0.697850494793315796932292388026640d0 /
  data xgk ( 17) / 0.660061064126626961370053668149271d0 /
  data xgk ( 18) / 0.620526182989242861140477556431189d0 /
  data xgk ( 19) / 0.579345235826361691756024932172540d0 /
  data xgk ( 20) / 0.536624148142019899264169793311073d0 /
  data xgk ( 21) / 0.492480467861778574993693061207709d0 /
  data xgk ( 22) / 0.447033769538089176780609900322854d0 /
  data xgk ( 23) / 0.400401254830394392535476211542661d0 /
  data xgk ( 24) / 0.352704725530878113471037207089374d0 /
  data xgk ( 25) / 0.304073202273625077372677107199257d0 /
  data xgk ( 26) / 0.254636926167889846439805129817805d0 /
  data xgk ( 27) / 0.204525116682309891438957671002025d0 /
  data xgk ( 28) / 0.153869913608583546963794672743256d0 /
  data xgk ( 29) / 0.102806937966737030147096751318001d0 /
  data xgk ( 30) / 0.051471842555317695833025213166723d0 /
  data xgk ( 31) / 0.000000000000000000000000000000000d0 /

  data wgk (  1) / 0.001389013698677007624551591226760d0 /
  data wgk (  2) / 0.003890461127099884051267201844516d0 /
  data wgk (  3) / 0.006630703915931292173319826369750d0 /
  data wgk (  4) / 0.009273279659517763428441146892024d0 /
  data wgk (  5) / 0.011823015253496341742232898853251d0 /
  data wgk (  6) / 0.014369729507045804812451432443580d0 /
  data wgk (  7) / 0.016920889189053272627572289420322d0 /
  data wgk (  8) / 0.019414141193942381173408951050128d0 /
  data wgk (  9) / 0.021828035821609192297167485738339d0 /
  data wgk ( 10) / 0.024191162078080601365686370725232d0 /
  data wgk ( 11) / 0.026509954882333101610601709335075d0 /
  data wgk ( 12) / 0.028754048765041292843978785354334d0 /
  data wgk ( 13) / 0.030907257562387762472884252943092d0 /
  data wgk ( 14) / 0.032981447057483726031814191016854d0 /
  data wgk ( 15) / 0.034979338028060024137499670731468d0 /
  data wgk ( 16) / 0.036882364651821229223911065617136d0 /
  data wgk ( 17) / 0.038678945624727592950348651532281d0 /
  data wgk ( 18) / 0.040374538951535959111995279752468d0 /
  data wgk ( 19) / 0.041969810215164246147147541285970d0 /
  data wgk ( 20) / 0.043452539701356069316831728117073d0 /
  data wgk ( 21) / 0.044814800133162663192355551616723d0 /
  data wgk ( 22) / 0.046059238271006988116271735559374d0 /
  data wgk ( 23) / 0.047185546569299153945261478181099d0 /
  data wgk ( 24) / 0.048185861757087129140779492298305d0 /
  data wgk ( 25) / 0.049055434555029778887528165367238d0 /
  data wgk ( 26) / 0.049795683427074206357811569379942d0 /
  data wgk ( 27) / 0.050405921402782346840893085653585d0 /
  data wgk ( 28) / 0.050881795898749606492297473049805d0 /
  data wgk ( 29) / 0.051221547849258772170656282604944d0 /
  data wgk ( 30) / 0.051426128537459025933862879215781d0 /
  data wgk ( 31) / 0.051494729429451567558340433647099d0 /

  epmach = epsilon ( epmach )
  uflow = tiny ( uflow )
  centr = 0.5D+00*(b+a)
  hlgth = 0.5D+00*(b-a)
  dhlgth =  abs ( hlgth)
!
!  compute the 61-point kronrod approximation to the
!  integral, and estimate the absolute error.
!
  resg = 0.0D+00
  fc = f(centr)
  resk = wgk(31)*fc
  resabs =  abs ( resk)

  do j=1,15
    jtw = j*2
    dabsc = hlgth*xgk(jtw)
    fval1 = f(centr-dabsc)
    fval2 = f(centr+dabsc)
    fv1(jtw) = fval1
    fv2(jtw) = fval2
    fsum = fval1+fval2
    resg = resg+wg(j)*fsum
    resk = resk+wgk(jtw)*fsum
    resabs = resabs+wgk(jtw)*( abs ( fval1)+ abs ( fval2))
  end do

  do j=1,15
    jtwm1 = j*2-1
    dabsc = hlgth*xgk(jtwm1)
    fval1 = f(centr-dabsc)
    fval2 = f(centr+dabsc)
    fv1(jtwm1) = fval1
    fv2(jtwm1) = fval2
    fsum = fval1+fval2
    resk = resk+wgk(jtwm1)*fsum
    resabs = resabs+wgk(jtwm1)*( abs ( fval1)+ abs ( fval2))
  end do

  reskh = resk*0.5D+00
  resasc = wgk(31)* abs ( fc-reskh)

  do j=1,30
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

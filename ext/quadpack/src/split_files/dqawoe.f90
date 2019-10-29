subroutine dqawoe ( f, a, b, omega, integr, epsabs, epsrel, limit, icall, &
  maxp1, result, abserr, neval, ier, last, alist, blist, rlist, elist, iord, &
  nnlog, momcom, chebmo )

!*****************************************************************************80
!
!! DQAWOE computes the integrals of oscillatory integrands.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  the routine calculates an approximation result to a given
!      definite integral
!      i = integral of f(x)*w(x) over (a,b)
!      where w(x) = cos(omega*x) or w(x)=sin(omega*x),
!      hopefully satisfying following claim for accuracy
!      abs(i-result).le.max(epsabs,epsrel*abs(i)).
!
!  Parameters:
!
!   on entry
!      f      - real ( kind = 8 )
!               function subprogram defining the integrand
!               function f(x). the actual name for f needs to be
!               declared e x t e r n a l in the driver program.
!
!      a      - real ( kind = 8 )
!               lower limit of integration
!
!      b      - real ( kind = 8 )
!               upper limit of integration
!
!      omega  - real ( kind = 8 )
!               parameter in the integrand weight function
!
!      integr - integer ( kind = 4 )
!               indicates which of the weight functions is to be
!               used
!               integr = 1      w(x) = cos(omega*x)
!               integr = 2      w(x) = sin(omega*x)
!               if integr.ne.1 and integr.ne.2, the routine
!               will end with ier = 6.
!
!      epsabs - real ( kind = 8 )
!               absolute accuracy requested
!      epsrel - real ( kind = 8 )
!               relative accuracy requested
!               if  epsabs.le.0
!               and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!               the routine will end with ier = 6.
!
!      limit  - integer ( kind = 4 )
!               gives an upper bound on the number of subdivisions
!               in the partition of (a,b), limit.ge.1.
!
!      icall  - integer ( kind = 4 )
!               if dqawoe is to be used only once, icall must
!               be set to 1.  assume that during this call, the
!               chebyshev moments (for clenshaw-curtis integration
!               of degree 24) have been computed for intervals of
!               lenghts (abs(b-a))*2**(-l), l=0,1,2,...momcom-1.
!               if icall.gt.1 this means that dqawoe has been
!               called twice or more on intervals of the same
!               length abs(b-a). the chebyshev moments already
!               computed are then re-used in subsequent calls.
!               if icall.lt.1, the routine will end with ier = 6.
!
!      maxp1  - integer ( kind = 4 )
!               gives an upper bound on the number of chebyshev
!               moments which can be stored, i.e. for the
!               intervals of lenghts abs(b-a)*2**(-l),
!               l=0,1, ..., maxp1-2, maxp1.ge.1.
!               if maxp1.lt.1, the routine will end with ier = 6.
!
!   on return
!      result - real ( kind = 8 )
!               approximation to the integral
!
!      abserr - real ( kind = 8 )
!               estimate of the modulus of the absolute error,
!               which should equal or exceed abs(i-result)
!
!      neval  - integer ( kind = 4 )
!               number of integrand evaluations
!
!      ier    - integer ( kind = 4 )
!               ier = 0 normal and reliable termination of the
!                       routine. it is assumed that the
!                       requested accuracy has been achieved.
!             - ier.gt.0 abnormal termination of the routine.
!                       the estimates for integral and error are
!                       less reliable. it is assumed that the
!                       requested accuracy has not been achieved.
!      error messages
!               ier = 1 maximum number of subdivisions allowed
!                       has been achieved. one can allow more
!                       subdivisions by increasing the value of
!                       limit (and taking according dimension
!                       adjustments into account). however, if
!                       this yields no improvement it is advised
!                       to analyze the integrand, in order to
!                       determine the integration difficulties.
!                       if the position of a local difficulty can
!                       be determined (e.g. singularity,
!                       discontinuity within the interval) one
!                       will probably gain from splitting up the
!                       interval at this point and calling the
!                       integrator on the subranges. if possible,
!                       an appropriate special-purpose integrator
!                       should be used which is designed for
!                       handling the type of difficulty involved.
!                   = 2 the occurrence of roundoff error is
!                       detected, which prevents the requested
!                       tolerance from being achieved.
!                       the error may be under-estimated.
!                   = 3 extremely bad integrand behaviour occurs
!                       at some points of the integration
!                       interval.
!                   = 4 the algorithm does not converge.
!                       roundoff error is detected in the
!                       extrapolation table.
!                       it is presumed that the requested
!                       tolerance cannot be achieved due to
!                       roundoff in the extrapolation table,
!                       and that the returned result is the
!                       best which can be obtained.
!                   = 5 the integral is probably divergent, or
!                       slowly convergent. it must be noted that
!                       divergence can occur with any other value
!                       of ier.gt.0.
!                   = 6 the input is invalid, because
!                       (epsabs.le.0 and
!                        epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
!                       or (integr.ne.1 and integr.ne.2) or
!                       icall.lt.1 or maxp1.lt.1.
!                       result, abserr, neval, last, rlist(1),
!                       elist(1), iord(1) and nnlog(1) are set
!                       to zero. alist(1) and blist(1) are set
!                       to a and b respectively.
!
!      last  -  integer ( kind = 4 )
!               on return, last equals the number of
!               subintervals produces in the subdivision
!               process, which determines the number of
!               significant elements actually in the
!               work arrays.
!      alist  - real ( kind = 8 )
!               vector of dimension at least limit, the first
!                last  elements of which are the left
!               end points of the subintervals in the partition
!               of the given integration range (a,b)
!
!      blist  - real ( kind = 8 )
!               vector of dimension at least limit, the first
!                last  elements of which are the right
!               end points of the subintervals in the partition
!               of the given integration range (a,b)
!
!      rlist  - real ( kind = 8 )
!               vector of dimension at least limit, the first
!                last  elements of which are the integral
!               approximations on the subintervals
!
!      elist  - real ( kind = 8 )
!               vector of dimension at least limit, the first
!                last  elements of which are the moduli of the
!               absolute error estimates on the subintervals
!
!      iord   - integer ( kind = 4 )
!               vector of dimension at least limit, the first k
!               elements of which are pointers to the error
!               estimates over the subintervals,
!               such that elist(iord(1)), ...,
!               elist(iord(k)) form a decreasing sequence, with
!               k = last if last.le.(limit/2+2), and
!               k = limit+1-last otherwise.
!
!      nnlog  - integer ( kind = 4 )
!               vector of dimension at least limit, containing the
!               subdivision levels of the subintervals, i.e.
!               iwork(i) = l means that the subinterval
!               numbered i is of length abs(b-a)*2**(1-l)
!
!   on entry and return
!      momcom - integer ( kind = 4 )
!               indicating that the chebyshev moments
!               have been computed for intervals of lengths
!               (abs(b-a))*2**(-l), l=0,1,2, ..., momcom-1,
!               momcom.lt.maxp1
!
!      chebmo - real ( kind = 8 )
!               array of dimension (maxp1,25) containing the
!               chebyshev moments
!
!  Local Parameters:
!
!      the dimension of rlist2 is determined by  the value of
!      limexp in routine dqelg (rlist2 should be of
!      dimension (limexp+2) at least).
!
!      list of major variables
!
!     alist     - list of left end points of all subintervals
!                 considered up to now
!     blist     - list of right end points of all subintervals
!                 considered up to now
!     rlist(i)  - approximation to the integral over
!                 (alist(i),blist(i))
!     rlist2    - array of dimension at least limexp+2
!                 containing the part of the epsilon table
!                 which is still needed for further computations
!     elist(i)  - error estimate applying to rlist(i)
!     maxerr    - pointer to the interval with largest
!                 error estimate
!     errmax    - elist(maxerr)
!     erlast    - error on the interval currently subdivided
!     area      - sum of the integrals over the subintervals
!     errsum    - sum of the errors over the subintervals
!     errbnd    - requested accuracy max(epsabs,epsrel*
!                 abs(result))
!     *****1    - variable for the left subinterval
!     *****2    - variable for the right subinterval
!     last      - index for subdivision
!     nres      - number of calls to the extrapolation routine
!     numrl2    - number of elements in rlist2. if an appropriate
!                 approximation to the compounded integral has
!                 been obtained it is put in rlist2(numrl2) after
!                 numrl2 has been increased by one
!     small     - length of the smallest interval considered
!                 up to now, multiplied by 1.5
!     erlarg    - sum of the errors over the intervals larger
!                 than the smallest interval considered up to now
!     extrap    - logical variable denoting that the routine is
!                 attempting to perform extrapolation, i.e. before
!                 subdividing the smallest interval we try to
!                 decrease the value of erlarg
!     noext     - logical variable denoting that extrapolation
!                 is no longer allowed (true  value)
!
!      machine dependent constants
!
!     epmach is the largest relative spacing.
!     uflow is the smallest positive magnitude.
!     oflow is the largest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,abseps,abserr,alist,area,area1,area12,area2,a1, &
    a2,b,blist,b1,b2,chebmo,correc,defab1,defab2,defabs, &
    domega,dres,elist,epmach,epsabs,epsrel,erlarg,erlast, &
    errbnd,errmax,error1,erro12,error2,errsum,ertest,f,oflow, &
    omega,resabs,reseps,result,res3la,rlist,rlist2,small,uflow,width
  integer ( kind = 4 ) icall,id,ier,ierro,integr,iord,iroff1,iroff2
  integer ( kind = 4 ) iroff3
  integer ( kind = 4 ) jupbnd
  integer ( kind = 4 ) k,ksgn,ktmin,last,limit,maxerr,maxp1,momcom,nev,neval, &
    nnlog,nres,nrmax,nrmom,numrl2
  logical extrap,noext,extall

  dimension alist(limit),blist(limit),rlist(limit),elist(limit), &
    iord(limit),rlist2(52),res3la(3),chebmo(maxp1,25),nnlog(limit)
  external f

  epmach = epsilon ( epmach )
!
!  test on validity of parameters
!
  ier = 0
  neval = 0
  last = 0
  result = 0.0D+00
  abserr = 0.0D+00
  alist(1) = a
  blist(1) = b
  rlist(1) = 0.0D+00
  elist(1) = 0.0D+00
  iord(1) = 0
  nnlog(1) = 0
  if((integr.ne.1.and.integr.ne.2).or.(epsabs.le.0.0D+00.and. &
    epsrel.lt. max ( 0.5D+02*epmach,0.5D-28)).or.icall.lt.1.or. &
    maxp1.lt.1) ier = 6
  if(ier.eq.6) go to 999
!
!  first approximation to the integral
!
  domega =  abs ( omega)
  nrmom = 0
  if (icall.gt.1) go to 5
  momcom = 0
    5 call dqc25f(f,a,b,domega,integr,nrmom,maxp1,0,result,abserr, &
    neval,defabs,resabs,momcom,chebmo)
!
!  test on accuracy.
!
  dres =  abs ( result)
  errbnd =  max ( epsabs,epsrel*dres)
  rlist(1) = result
  elist(1) = abserr
  iord(1) = 1
  if(abserr.le.0.1D+03*epmach*defabs.and.abserr.gt.errbnd) ier = 2
  if(limit.eq.1) ier = 1
  if(ier.ne.0.or.abserr.le.errbnd) go to 200
!
!  initializations
!
  uflow = tiny ( uflow )
  oflow = huge ( oflow )
  errmax = abserr
  maxerr = 1
  area = result
  errsum = abserr
  abserr = oflow
  nrmax = 1
  extrap = .false.
  noext = .false.
  ierro = 0
  iroff1 = 0
  iroff2 = 0
  iroff3 = 0
  ktmin = 0
  small =  abs ( b-a)*0.75D+00
  nres = 0
  numrl2 = 0
  extall = .false.
  if(0.5D+00* abs ( b-a)*domega.gt.0.2D+01) go to 10
  numrl2 = 1
  extall = .true.
  rlist2(1) = result
   10 if(0.25D+00* abs ( b-a)*domega.le.0.2D+01) extall = .true.
  ksgn = -1
  if(dres.ge.(0.1D+01-0.5D+02*epmach)*defabs) ksgn = 1
!
!  main do-loop
!
  do 140 last = 2,limit
!
!  bisect the subinterval with the nrmax-th largest error estimate.
!
    nrmom = nnlog(maxerr)+1
    a1 = alist(maxerr)
    b1 = 0.5D+00*(alist(maxerr)+blist(maxerr))
    a2 = b1
    b2 = blist(maxerr)
    erlast = errmax
    call dqc25f(f,a1,b1,domega,integr,nrmom,maxp1,0, &
    area1,error1,nev,resabs,defab1,momcom,chebmo)
    neval = neval+nev
    call dqc25f(f,a2,b2,domega,integr,nrmom,maxp1,1, &
    area2,error2,nev,resabs,defab2,momcom,chebmo)
    neval = neval+nev
!
!  improve previous approximations to integral
!  and error and test for accuracy.
!
    area12 = area1+area2
    erro12 = error1+error2
    errsum = errsum+erro12-errmax
    area = area+area12-rlist(maxerr)
    if(defab1.eq.error1.or.defab2.eq.error2) go to 25
    if( abs ( rlist(maxerr)-area12).gt.0.1D-04* abs ( area12) &
    .or.erro12.lt.0.99D+00*errmax) go to 20
    if(extrap) iroff2 = iroff2+1
    if(.not.extrap) iroff1 = iroff1+1
   20   if(last.gt.10.and.erro12.gt.errmax) iroff3 = iroff3+1
   25   rlist(maxerr) = area1
    rlist(last) = area2
    nnlog(maxerr) = nrmom
    nnlog(last) = nrmom
    errbnd =  max ( epsabs,epsrel* abs ( area))
!
!  test for roundoff error and eventually set error flag.
!
    if(iroff1+iroff2.ge.10.or.iroff3.ge.20) ier = 2
    if(iroff2.ge.5) ierro = 3
!
!  set error flag in the case that the number of
!  subintervals equals limit.
!
    if(last.eq.limit) ier = 1
!
!  set error flag in the case of bad integrand behaviour
!  at a point of the integration range.
!
    if( max (  abs ( a1), abs ( b2)).le.(0.1D+01+0.1D+03*epmach) &
    *( abs ( a2)+0.1D+04*uflow)) ier = 4
!
!  append the newly-created intervals to the list.
!
    if(error2.gt.error1) go to 30
    alist(last) = a2
    blist(maxerr) = b1
    blist(last) = b2
    elist(maxerr) = error1
    elist(last) = error2
    go to 40
   30   alist(maxerr) = a2
    alist(last) = a1
    blist(last) = b1
    rlist(maxerr) = area2
    rlist(last) = area1
    elist(maxerr) = error2
    elist(last) = error1
!
!  call dqpsrt to maintain the descending ordering
!  in the list of error estimates and select the subinterval
!  with nrmax-th largest error estimate (to bisected next).
!
   40   call dqpsrt(limit,last,maxerr,errmax,elist,iord,nrmax)
  if(errsum.le.errbnd) go to 170
  if(ier.ne.0) go to 150
    if(last.eq.2.and.extall) go to 120
    if(noext) go to 140
    if(.not.extall) go to 50
    erlarg = erlarg-erlast
    if( abs ( b1-a1).gt.small) erlarg = erlarg+erro12
    if(extrap) go to 70
!
!  test whether the interval to be bisected next is the
!  smallest interval.
!
   50   width =  abs ( blist(maxerr)-alist(maxerr))
    if(width.gt.small) go to 140
    if(extall) go to 60
!
!  test whether we can start with the extrapolation procedure
!  (we do this if we integrate over the next interval with
!  use of a gauss-kronrod rule - see routine dqc25f).
!
    small = small*0.5D+00
    if(0.25D+00*width*domega.gt.0.2D+01) go to 140
    extall = .true.
    go to 130
   60   extrap = .true.
    nrmax = 2
   70   if(ierro.eq.3.or.erlarg.le.ertest) go to 90
!
!  the smallest interval has the largest error.
!  before bisecting decrease the sum of the errors over
!  the larger intervals (erlarg) and perform extrapolation.
!
    jupbnd = last
    if (last.gt.(limit/2+2)) jupbnd = limit+3-last
    id = nrmax
    do k = id,jupbnd
      maxerr = iord(nrmax)
      errmax = elist(maxerr)
      if( abs ( blist(maxerr)-alist(maxerr)).gt.small) go to 140
      nrmax = nrmax+1
    end do
!
!  perform extrapolation.
!
   90   numrl2 = numrl2+1
    rlist2(numrl2) = area
    if(numrl2.lt.3) go to 110
    call dqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
    ktmin = ktmin+1
    if(ktmin.gt.5.and.abserr.lt.0.1D-02*errsum) ier = 5
    if(abseps.ge.abserr) go to 100
    ktmin = 0
    abserr = abseps
    result = reseps
    correc = erlarg
    ertest =  max ( epsabs,epsrel* abs ( reseps))
    if(abserr.le.ertest) go to 150
!
!  prepare bisection of the smallest interval.
!
  100   if(numrl2.eq.1) noext = .true.
    if(ier.eq.5) go to 150
  110   maxerr = iord(1)
    errmax = elist(maxerr)
    nrmax = 1
    extrap = .false.
    small = small*0.5D+00
    erlarg = errsum
    go to 140
  120   small = small*0.5D+00
    numrl2 = numrl2+1
    rlist2(numrl2) = area
  130   ertest = errbnd
    erlarg = errsum
  140 continue
!
!  set the final result.-
!
  150 if(abserr.eq.oflow.or.nres.eq.0) go to 170
  if(ier+ierro.eq.0) go to 165
  if(ierro.eq.3) abserr = abserr+correc
  if(ier.eq.0) ier = 3
  if(result.ne.0.0D+00.and.area.ne.0.0D+00) go to 160
  if(abserr.gt.errsum) go to 170
  if(area.eq.0.0D+00) go to 190
  go to 165
  160 if(abserr/ abs ( result).gt.errsum/ abs ( area)) go to 170
!
!  test on divergence.
!
  165 if(ksgn.eq.(-1).and. max (  abs ( result), abs ( area)).le. &
   defabs*0.1D-01) go to 190
  if(0.1D-01.gt.(result/area).or.(result/area).gt.0.1D+03 &
   .or.errsum.ge. abs ( area)) ier = 6
  go to 190
!
!  compute global integral sum.
!
  170 result = 0.0D+00
  do k=1,last
    result = result+rlist(k)
  end do
  abserr = errsum
  190 if (ier.gt.2) ier=ier-1
  200 if (integr.eq.2.and.omega.lt.0.0D+00) result=-result
  999 continue

  return
end

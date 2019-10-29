subroutine dqagpe(f,a,b,npts2,points,epsabs,epsrel,limit,result, &
  abserr,neval,ier,alist,blist,rlist,elist,pts,iord,level,ndin, &
  last)

!*****************************************************************************80
!
!! DQAGPE computes a definite integral.
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
!      definite integral i = integral of f over (a,b), hopefully
!      satisfying following claim for accuracy abs(i-result).le.
!      max(epsabs,epsrel*abs(i)). break points of the integration
!      interval, where local difficulties of the integrand may
!      occur(e.g. singularities,discontinuities),provided by user.
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
!      npts2  - integer ( kind = 4 )
!               number equal to two more than the number of
!               user-supplied break points within the integration
!               range, npts2.ge.2.
!               if npts2.lt.2, the routine will end with ier = 6.
!
!      points - real ( kind = 8 )
!               vector of dimension npts2, the first (npts2-2)
!               elements of which are the user provided break
!               points. if these points do not constitute an
!               ascending sequence there will be an automatic
!               sorting.
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
!               gives an upper bound on the number of subintervals
!               in the partition of (a,b), limit.ge.npts2
!               if limit.lt.npts2, the routine will end with
!               ier = 6.
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
!                       routine. it is assumed that the requested
!                       accuracy has been achieved.
!               ier.gt.0 abnormal termination of the routine.
!                       the estimates for integral and error are
!                       less reliable. it is assumed that the
!                       requested accuracy has not been achieved.
!      error messages
!               ier = 1 maximum number of subdivisions allowed
!                       has been achieved. one can allow more
!                       subdivisions by increasing the value of
!                       limit (and taking the according dimension
!                       adjustments into account). however, if
!                       this yields no improvement it is advised
!                       to analyze the integrand in order to
!                       determine the integration difficulties. if
!                       the position of a local difficulty can be
!                       determined (i.e. singularity,
!                       discontinuity within the interval), it
!                       should be supplied to the routine as an
!                       element of the vector points. if necessary
!                       an appropriate special-purpose integrator
!                       must be used, which is designed for
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
!                       extrapolation table. it is presumed that
!                       the requested tolerance cannot be
!                       achieved, and that the returned result is
!                       the best which can be obtained.
!                   = 5 the integral is probably divergent, or
!                       slowly convergent. it must be noted that
!                       divergence can occur with any other value
!                       of ier.gt.0.
!                   = 6 the input is invalid because
!                       npts2.lt.2 or
!                       break points are specified outside
!                       the integration range or
!                       (epsabs.le.0 and
!                        epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
!                       or limit.lt.npts2.
!                       result, abserr, neval, last, rlist(1),
!                       and elist(1) are set to zero. alist(1) and
!                       blist(1) are set to a and b respectively.
!
!      alist  - real ( kind = 8 )
!               vector of dimension at least limit, the first
!                last  elements of which are the left end points
!               of the subintervals in the partition of the given
!               integration range (a,b)
!
!      blist  - real ( kind = 8 )
!               vector of dimension at least limit, the first
!                last  elements of which are the right end points
!               of the subintervals in the partition of the given
!               integration range (a,b)
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
!      pts    - real ( kind = 8 )
!               vector of dimension at least npts2, containing the
!               integration limits and the break points of the
!               interval in ascending sequence.
!
!      level  - integer ( kind = 4 )
!               vector of dimension at least limit, containing the
!               subdivision levels of the subinterval, i.e. if
!               (aa,bb) is a subinterval of (p1,p2) where p1 as
!               well as p2 is a user-provided break point or
!               integration limit, then (aa,bb) has level l if
!               abs(bb-aa) = abs(p2-p1)*2**(-l).
!
!      ndin   - integer ( kind = 4 )
!               vector of dimension at least npts2, after first
!               integration over the intervals (pts(i)),pts(i+1),
!               i = 0,1, ..., npts2-2, the error estimates over
!               some of the intervals may have been increased
!               artificially, in order to put their subdivision
!               forward. if this happens for the subinterval
!               numbered k, ndin(k) is put to 1, otherwise
!               ndin(k) = 0.
!
!      iord   - integer ( kind = 4 )
!               vector of dimension at least limit, the first k
!               elements of which are pointers to the
!               error estimates over the subintervals,
!               such that elist(iord(1)), ..., elist(iord(k))
!               form a decreasing sequence, with k = last
!               if last.le.(limit/2+2), and k = limit+1-last
!               otherwise
!
!      last   - integer ( kind = 4 )
!               number of subintervals actually produced in the
!               subdivisions process
!
!  Local Parameters:
!
!      the dimension of rlist2 is determined by the value of
!      limexp in routine epsalg (rlist2 should be of dimension
!      (limexp+2) at least).
!
!     alist     - list of left end points of all subintervals
!                 considered up to now
!     blist     - list of right end points of all subintervals
!                 considered up to now
!     rlist(i)  - approximation to the integral over
!                 (alist(i),blist(i))
!     rlist2    - array of dimension at least limexp+2
!                 containing the part of the epsilon table which
!                 is still needed for further computations
!     elist(i)  - error estimate applying to rlist(i)
!     maxerr    - pointer to the interval with largest error
!                 estimate
!     errmax    - elist(maxerr)
!     erlast    - error on the interval currently subdivided
!                 (before that subdivision has taken place)
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
!                 been obtained, it is put in rlist2(numrl2) after
!                 numrl2 has been increased by one.
!     erlarg    - sum of the errors over the intervals larger
!                 than the smallest interval considered up to now
!     extrap    - logical variable denoting that the routine
!                 is attempting to perform extrapolation. i.e.
!                 before subdividing the smallest interval we
!                 try to decrease the value of erlarg.
!     noext     - logical variable denoting that extrapolation is
!                 no longer allowed (true-value)
!
!      machine dependent constants
!
!     epmach is the largest relative spacing.
!     uflow is the smallest positive magnitude.
!     oflow is the largest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,abseps,abserr,alist,area,area1,area12,area2,a1, &
    a2,b,blist,b1,b2,correc,defabs,defab1,defab2, &
    dres,elist,epmach,epsabs,epsrel,erlarg,erlast,errbnd, &
    errmax,error1,erro12,error2,errsum,ertest,f,oflow,points,pts, &
    resa,resabs,reseps,result,res3la,rlist,rlist2,sgn,temp,uflow
  integer ( kind = 4 ) i,id,ier,ierro,ind1,ind2,iord,ip1, &
    iroff1,iroff2,iroff3,j, &
    jlow,jupbnd,k,ksgn,ktmin,last,levcur,level,levmax,limit,maxerr, &
    ndin,neval,nint,nintp1,npts,npts2,nres,nrmax,numrl2
  logical extrap,noext

  dimension alist(limit),blist(limit),elist(limit),iord(limit), &
    level(limit),ndin(npts2),points(npts2),pts(npts2),res3la(3), &
    rlist(limit),rlist2(52)

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
  level(1) = 0
  npts = npts2-2
  if(npts2.lt.2.or.limit.le.npts.or.(epsabs.le.0.0D+00.and. &
    epsrel.lt. max ( 0.5D+02*epmach,0.5d-28))) ier = 6

  if(ier.eq.6) then
    return
  end if
!
!  if any break points are provided, sort them into an
!  ascending sequence.
!
  sgn = 1.0D+00
  if(a.gt.b) sgn = -1.0D+00
  pts(1) =  min (a,b)
  if(npts.eq.0) go to 15
  do i = 1,npts
    pts(i+1) = points(i)
  end do
   15 pts(npts+2) =  max ( a,b)
  nint = npts+1
  a1 = pts(1)
  if(npts.eq.0) go to 40
  nintp1 = nint+1
  do i = 1,nint
    ip1 = i+1
    do j = ip1,nintp1
      if(pts(i).gt.pts(j)) then
        temp = pts(i)
        pts(i) = pts(j)
        pts(j) = temp
      end if
    end do
  end do
  if(pts(1).ne. min (a,b).or.pts(nintp1).ne. max ( a,b)) ier = 6

  if(ier.eq.6) then
    return
  end if
!
!  compute first integral and error approximations.
!
   40 resabs = 0.0D+00

  do i = 1,nint
    b1 = pts(i+1)
    call dqk21(f,a1,b1,area1,error1,defabs,resa)
    abserr = abserr+error1
    result = result+area1
    ndin(i) = 0
    if(error1.eq.resa.and.error1.ne.0.0D+00) ndin(i) = 1
    resabs = resabs+defabs
    level(i) = 0
    elist(i) = error1
    alist(i) = a1
    blist(i) = b1
    rlist(i) = area1
    iord(i) = i
    a1 = b1
  end do

  errsum = 0.0D+00
  do i = 1,nint
    if(ndin(i).eq.1) elist(i) = abserr
    errsum = errsum+elist(i)
  end do
!
!  test on accuracy.
!
  last = nint
  neval = 21*nint
  dres =  abs ( result)
  errbnd =  max ( epsabs,epsrel*dres)
  if(abserr.le.0.1D+03*epmach*resabs.and.abserr.gt.errbnd) ier = 2
  if(nint.eq.1) go to 80

  do i = 1,npts
    jlow = i+1
    ind1 = iord(i)
    do j = jlow,nint
      ind2 = iord(j)
      if(elist(ind1).le.elist(ind2)) then
        ind1 = ind2
        k = j
      end if
    end do
    if(ind1.ne.iord(i)) then
      iord(k) = iord(i)
      iord(i) = ind1
    end if
  end do

  if(limit.lt.npts2) ier = 1
   80 if(ier.ne.0.or.abserr.le.errbnd) go to 210
!
!  initialization
!
  rlist2(1) = result
  maxerr = iord(1)
  errmax = elist(maxerr)
  area = result
  nrmax = 1
  nres = 0
  numrl2 = 1
  ktmin = 0
  extrap = .false.
  noext = .false.
  erlarg = errsum
  ertest = errbnd
  levmax = 1
  iroff1 = 0
  iroff2 = 0
  iroff3 = 0
  ierro = 0
  uflow = tiny ( uflow )
  oflow = huge ( oflow )
  abserr = oflow
  ksgn = -1
  if(dres.ge.(0.1D+01-0.5D+02*epmach)*resabs) ksgn = 1
!
!  main do-loop
!
  do 160 last = npts2,limit
!
!  bisect the subinterval with the nrmax-th largest error estimate.
!
    levcur = level(maxerr)+1
    a1 = alist(maxerr)
    b1 = 0.5D+00*(alist(maxerr)+blist(maxerr))
    a2 = b1
    b2 = blist(maxerr)
    erlast = errmax
    call dqk21(f,a1,b1,area1,error1,resa,defab1)
    call dqk21(f,a2,b2,area2,error2,resa,defab2)
!
!  improve previous approximations to integral
!  and error and test for accuracy.
!
    neval = neval+42
    area12 = area1+area2
    erro12 = error1+error2
    errsum = errsum+erro12-errmax
    area = area+area12-rlist(maxerr)
    if(defab1.eq.error1.or.defab2.eq.error2) go to 95
    if( abs ( rlist(maxerr)-area12).gt.0.1D-04* abs ( area12) &
    .or.erro12.lt.0.99D+00*errmax) go to 90
    if(extrap) iroff2 = iroff2+1
    if(.not.extrap) iroff1 = iroff1+1
   90   if(last.gt.10.and.erro12.gt.errmax) iroff3 = iroff3+1
   95   level(maxerr) = levcur
    level(last) = levcur
    rlist(maxerr) = area1
    rlist(last) = area2
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
!  at a point of the integration range
!
    if( max (  abs ( a1), abs ( b2)).le.(0.1D+01+0.1D+03*epmach)* &
    ( abs ( a2)+0.1D+04*uflow)) ier = 4
!
!  append the newly-created intervals to the list.
!
    if(error2.gt.error1) go to 100
    alist(last) = a2
    blist(maxerr) = b1
    blist(last) = b2
    elist(maxerr) = error1
    elist(last) = error2
    go to 110
  100   alist(maxerr) = a2
    alist(last) = a1
    blist(last) = b1
    rlist(maxerr) = area2
    rlist(last) = area1
    elist(maxerr) = error2
    elist(last) = error1
!
!  call dqpsrt to maintain the descending ordering
!  in the list of error estimates and select the subinterval
!  with nrmax-th largest error estimate (to be bisected next).
!
  110   call dqpsrt(limit,last,maxerr,errmax,elist,iord,nrmax)
    if(errsum.le.errbnd) go to 190
    if(ier.ne.0) go to 170
    if(noext) go to 160
    erlarg = erlarg-erlast
    if(levcur+1.le.levmax) erlarg = erlarg+erro12
    if(extrap) go to 120
!
!     test whether the interval to be bisected next is the
!     smallest interval.
!
    if(level(maxerr)+1.le.levmax) go to 160
    extrap = .true.
    nrmax = 2
  120   if(ierro.eq.3.or.erlarg.le.ertest) go to 140
!
!  the smallest interval has the largest error.
!  before bisecting decrease the sum of the errors over
!  the larger intervals (erlarg) and perform extrapolation.
!
    id = nrmax
    jupbnd = last
    if(last.gt.(2+limit/2)) jupbnd = limit+3-last

    do k = id,jupbnd
      maxerr = iord(nrmax)
      errmax = elist(maxerr)
      if(level(maxerr)+1.le.levmax) go to 160
      nrmax = nrmax+1
    end do
!
!  perform extrapolation.
!
  140   numrl2 = numrl2+1
    rlist2(numrl2) = area
    if(numrl2.le.2) go to 155
    call dqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
    ktmin = ktmin+1
    if(ktmin.gt.5.and.abserr.lt.0.1D-02*errsum) ier = 5
    if(abseps.ge.abserr) go to 150
    ktmin = 0
    abserr = abseps
    result = reseps
    correc = erlarg
    ertest =  max ( epsabs,epsrel* abs ( reseps))
    if(abserr.lt.ertest) go to 170
!
!  prepare bisection of the smallest interval.
!
  150   if(numrl2.eq.1) noext = .true.
    if(ier.ge.5) go to 170
  155   maxerr = iord(1)
    errmax = elist(maxerr)
    nrmax = 1
    extrap = .false.
    levmax = levmax+1
    erlarg = errsum
  160 continue
!
!  set the final result.
!
  170 continue

  if(abserr.eq.oflow) go to 190
  if((ier+ierro).eq.0) go to 180
  if(ierro.eq.3) abserr = abserr+correc
  if(ier.eq.0) ier = 3
  if(result.ne.0.0D+00.and.area.ne.0.0D+00)go to 175
  if(abserr.gt.errsum)go to 190
  if(area.eq.0.0D+00) go to 210
  go to 180
  175 if(abserr/ abs ( result).gt.errsum/ abs ( area))go to 190
!
!  test on divergence.
!
  180 if(ksgn.eq.(-1).and. max (  abs ( result), abs ( area)).le. &
    resabs*0.1D-01) go to 210
  if(0.1D-01.gt.(result/area).or.(result/area).gt.0.1D+03.or. &
    errsum.gt. abs ( area)) ier = 6
  go to 210
!
!  compute global integral sum.
!
  190 result = 0.0D+00
  do k = 1,last
    result = result+rlist(k)
  end do

  abserr = errsum
  210 if(ier.gt.2) ier = ier-1
  result = result*sgn

  return
end

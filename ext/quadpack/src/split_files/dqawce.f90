subroutine dqawce(f,a,b,c,epsabs,epsrel,limit,result,abserr,neval, &
  ier,alist,blist,rlist,elist,iord,last)

!*****************************************************************************80
!
!! DQAWCE computes a Cauchy principal value.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***  purpose  the routine calculates an approximation result to a
!        cauchy principal value i = integral of f*w over (a,b)
!        (w(x) = 1/(x-c), (c.ne.a, c.ne.b), hopefully satisfying
!        following claim for accuracy
!        abs(i-result).le.max(epsabs,epsrel*abs(i))
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
!      c      - real ( kind = 8 )
!               parameter in the weight function, c.ne.a, c.ne.b
!               if c = a or c = b, the routine will end with
!               ier = 6.
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
!               in the partition of (a,b), limit.ge.1
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
!               ier.gt.0 abnormal termination of the routine
!                       the estimates for integral and error are
!                       less reliable. it is assumed that the
!                       requested accuracy has not been achieved.
!      error messages
!               ier = 1 maximum number of subdivisions allowed
!                       has been achieved. one can allow more sub-
!                       divisions by increasing the value of
!                       limit. however, if this yields no
!                       improvement it is advised to analyze the
!                       the integrand, in order to determine the
!                       the integration difficulties. if the
!                       position of a local difficulty can be
!                       determined (e.g. singularity,
!                       discontinuity within the interval) one
!                       will probably gain from splitting up the
!                       interval at this point and calling
!                       appropriate integrators on the subranges.
!                   = 2 the occurrence of roundoff error is detec-
!                       ted, which prevents the requested
!                       tolerance from being achieved.
!                   = 3 extremely bad integrand behaviour
!                       occurs at some interior points of
!                       the integration interval.
!                   = 6 the input is invalid, because
!                       c = a or c = b or
!                       (epsabs.le.0 and
!                        epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
!                       or limit.lt.1.
!                       result, abserr, neval, rlist(1), elist(1),
!                       iord(1) and last are set to zero. alist(1)
!                       and blist(1) are set to a and b
!                       respectively.
!
!      alist   - real ( kind = 8 )
!                vector of dimension at least limit, the first
!                 last  elements of which are the left
!                end points of the subintervals in the partition
!                of the given integration range (a,b)
!
!      blist   - real ( kind = 8 )
!                vector of dimension at least limit, the first
!                 last  elements of which are the right
!                end points of the subintervals in the partition
!                of the given integration range (a,b)
!
!      rlist   - real ( kind = 8 )
!                vector of dimension at least limit, the first
!                 last  elements of which are the integral
!                approximations on the subintervals
!
!      elist   - real ( kind = 8 )
!                vector of dimension limit, the first  last
!                elements of which are the moduli of the absolute
!                error estimates on the subintervals
!
!      iord    - integer ( kind = 4 )
!                vector of dimension at least limit, the first k
!                elements of which are pointers to the error
!                estimates over the subintervals, so that
!                elist(iord(1)), ..., elist(iord(k)) with k = last
!                if last.le.(limit/2+2), and k = limit+1-last
!                otherwise, form a decreasing sequence
!
!      last    - integer ( kind = 4 )
!                number of subintervals actually produced in
!                the subdivision process
!
!  Local Parameters:
!
!     alist     - list of left end points of all subintervals
!                 considered up to now
!     blist     - list of right end points of all subintervals
!                 considered up to now
!     rlist(i)  - approximation to the integral over
!                 (alist(i),blist(i))
!     elist(i)  - error estimate applying to rlist(i)
!     maxerr    - pointer to the interval with largest
!                 error estimate
!     errmax    - elist(maxerr)
!     area      - sum of the integrals over the subintervals
!     errsum    - sum of the errors over the subintervals
!     errbnd    - requested accuracy max(epsabs,epsrel*
!                 abs(result))
!     *****1    - variable for the left subinterval
!     *****2    - variable for the right subinterval
!     last      - index for subdivision
!
!
!      machine dependent constants
!
!     epmach is the largest relative spacing.
!     uflow is the smallest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,aa,abserr,alist,area,area1,area12,area2,a1,a2, &
    b,bb,blist,b1,b2,c,elist,epmach,epsabs,epsrel, &
    errbnd,errmax,error1,erro12,error2,errsum,f,result,rlist,uflow
  integer ( kind = 4 ) ier,iord,iroff1,iroff2,k,krule,last,limit,&
    maxerr, nev, &
    neval,nrmax
  dimension alist(limit),blist(limit),rlist(limit),elist(limit), &
    iord(limit)

  external f

  epmach = epsilon ( epmach )
  uflow = tiny ( uflow )
!
!  test on validity of parameters
!
  ier = 6
  neval = 0
  last = 0
  alist(1) = a
  blist(1) = b
  rlist(1) = 0.0D+00
  elist(1) = 0.0D+00
  iord(1) = 0
  result = 0.0D+00
  abserr = 0.0D+00

  if ( c.eq.a .or. &
    c.eq.b .or. &
    (epsabs.le.0.0D+00 .and. epsrel.lt. max ( 0.5D+02*epmach,0.5d-28)) ) then
    ier = 6
    return
  end if
!
!  first approximation to the integral
!
  if ( a <= b ) then
    aa=a
    bb=b
  else
    aa=b
    bb=a
  end if

  ier=0
  krule = 1
  call dqc25c(f,aa,bb,c,result,abserr,krule,neval)
  last = 1
  rlist(1) = result
  elist(1) = abserr
  iord(1) = 1
  alist(1) = a
  blist(1) = b
!
!  test on accuracy
!
  errbnd =  max ( epsabs,epsrel* abs ( result))
  if(limit.eq.1) ier = 1

  if(abserr.lt. min (0.1D-01* abs ( result),errbnd) &
    .or.ier.eq.1) go to 70
!
!  initialization
!
  alist(1) = aa
  blist(1) = bb
  rlist(1) = result
  errmax = abserr
  maxerr = 1
  area = result
  errsum = abserr
  nrmax = 1
  iroff1 = 0
  iroff2 = 0
!
!  main do-loop
!
  do 40 last = 2,limit
!
!  bisect the subinterval with nrmax-th largest error estimate.
!
    a1 = alist(maxerr)
    b1 = 0.5D+00*(alist(maxerr)+blist(maxerr))
    b2 = blist(maxerr)
    if(c.le.b1.and.c.gt.a1) b1 = 0.5D+00*(c+b2)
    if(c.gt.b1.and.c.lt.b2) b1 = 0.5D+00*(a1+c)
    a2 = b1
    krule = 2
    call dqc25c(f,a1,b1,c,area1,error1,krule,nev)
    neval = neval+nev
    call dqc25c(f,a2,b2,c,area2,error2,krule,nev)
    neval = neval+nev
!
!  improve previous approximations to integral
!  and error and test for accuracy.
!
    area12 = area1+area2
    erro12 = error1+error2
    errsum = errsum+erro12-errmax
    area = area+area12-rlist(maxerr)
    if( abs ( rlist(maxerr)-area12).lt.0.1D-04* abs ( area12) &
      .and.erro12.ge.0.99D+00*errmax.and.krule.eq.0) &
      iroff1 = iroff1+1
    if(last.gt.10.and.erro12.gt.errmax.and.krule.eq.0) &
      iroff2 = iroff2+1
    rlist(maxerr) = area1
    rlist(last) = area2
    errbnd =  max ( epsabs,epsrel* abs ( area))
    if(errsum.le.errbnd) go to 15
!
!  test for roundoff error and eventually set error flag.
!
    if(iroff1.ge.6.and.iroff2.gt.20) ier = 2
!
!  set error flag in the case that number of interval bisections exceeds limit.
!
    if(last.eq.limit) ier = 1
!
!  set error flag in the case of bad integrand behaviour
!  at a point of the integration range.
!
    if( max (  abs ( a1), abs ( b2)).le.(0.1D+01+0.1D+03*epmach) &
      *( abs ( a2)+0.1D+04*uflow)) ier = 3
!
!  append the newly-created intervals to the list.
!
   15   continue

    if ( error2 .le. error1 ) then
      alist(last) = a2
      blist(maxerr) = b1
      blist(last) = b2
      elist(maxerr) = error1
      elist(last) = error2
    else
      alist(maxerr) = a2
      alist(last) = a1
      blist(last) = b1
      rlist(maxerr) = area2
      rlist(last) = area1
      elist(maxerr) = error2
      elist(last) = error1
    end if
!
!  call dqpsrt to maintain the descending ordering
!  in the list of error estimates and select the subinterval
!  with nrmax-th largest error estimate (to be bisected next).
!
    call dqpsrt(limit,last,maxerr,errmax,elist,iord,nrmax)

    if(ier.ne.0.or.errsum.le.errbnd) go to 50

   40 continue
!
!  compute final result.
!
   50 continue

  result = 0.0D+00
  do k=1,last
    result = result+rlist(k)
  end do

  abserr = errsum
   70 if (aa.eq.b) result=-result

  return
end

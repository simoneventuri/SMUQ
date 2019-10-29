subroutine dqage ( f, a, b, epsabs, epsrel, key, limit, result, abserr, &
  neval, ier, alist, blist, rlist, elist, iord, last )

!*****************************************************************************80
!
!! DQAGE estimates a definite integral.
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
!      definite integral   i = integral of f over (a,b),
!      hopefully satisfying following claim for accuracy
!      abs(i-reslt).le.max(epsabs,epsrel*abs(i)).
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
!      epsabs - real ( kind = 8 )
!               absolute accuracy requested
!      epsrel - real ( kind = 8 )
!               relative accuracy requested
!               if  epsabs.le.0
!               and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!               the routine will end with ier = 6.
!
!      key    - integer ( kind = 4 )
!               key for choice of local integration rule
!               a gauss-kronrod pair is used with
!                    7 - 15 points if key.lt.2,
!                   10 - 21 points if key = 2,
!                   15 - 31 points if key = 3,
!                   20 - 41 points if key = 4,
!                   25 - 51 points if key = 5,
!                   30 - 61 points if key.gt.5.
!
!      limit  - integer ( kind = 4 )
!               gives an upperbound on the number of subintervals
!               in the partition of (a,b), limit.ge.1.
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
!                       the estimates for result and error are
!                       less reliable. it is assumed that the
!                       requested accuracy has not been achieved.
!      error messages
!               ier = 1 maximum number of subdivisions allowed
!                       has been achieved. one can allow more
!                       subdivisions by increasing the value
!                       of limit.
!                       however, if this yields no improvement it
!                       is rather advised to analyze the integrand
!                       in order to determine the integration
!                       difficulties. if the position of a local
!                       difficulty can be determined(e.g.
!                       singularity, discontinuity within the
!                       interval) one will probably gain from
!                       splitting up the interval at this point
!                       and calling the integrator on the
!                       subranges. if possible, an appropriate
!                       special-purpose integrator should be used
!                       which is designed for handling the type of
!                       difficulty involved.
!                   = 2 the occurrence of roundoff error is
!                       detected, which prevents the requested
!                       tolerance from being achieved.
!                   = 3 extremely bad integrand behaviour occurs
!                       at some points of the integration
!                       interval.
!                   = 6 the input is invalid, because
!                       (epsabs.le.0 and
!                        epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!                       result, abserr, neval, last, rlist(1) ,
!                       elist(1) and iord(1) are set to zero.
!                       alist(1) and blist(1) are set to a and b
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
!                 last  elements of which are the
!                integral approximations on the subintervals
!
!      elist   - real ( kind = 8 )
!                vector of dimension at least limit, the first
!                 last  elements of which are the moduli of the
!                absolute error estimates on the subintervals
!
!      iord    - integer ( kind = 4 )
!                vector of dimension at least limit, the first k
!                elements of which are pointers to the
!                error estimates over the subintervals,
!                such that elist(iord(1)), ...,
!                elist(iord(k)) form a decreasing sequence,
!                with k = last if last.le.(limit/2+2), and
!                k = limit+1-last otherwise
!
!      last    - integer ( kind = 4 )
!                number of subintervals actually produced in the
!                subdivision process
!
!  Local Parameters:
!
!     alist     - list of left end points of all subintervals
!                 considered up to now
!     blist     - list of right end points of all subintervals
!                 considered up to now
!     rlist(i)  - approximation to the integral over
!                (alist(i),blist(i))
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
!     machine dependent constants
!
!     epmach  is the largest relative spacing.
!     uflow  is the smallest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,abserr,alist,area,area1,area12,area2,a1,a2,b, &
    blist,b1,b2,defabs,defab1,defab2,elist,epmach, &
    epsabs,epsrel,errbnd,errmax,error1,error2,erro12,errsum,f, &
    resabs,result,rlist,uflow
  integer ( kind = 4 ) ier,iord,iroff1,iroff2,k,key,keyf,last,limit, &
    maxerr, nrmax, neval

  dimension alist(limit),blist(limit),elist(limit),iord(limit), &
    rlist(limit)

  external f

  epmach = epsilon ( epmach )
  uflow = tiny ( uflow )
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

  if(epsabs.le.0.0D+00.and. &
    epsrel.lt. max ( 0.5D+02*epmach,0.5d-28)) then
    ier = 6
    return
  end if
!
!  first approximation to the integral
!
  keyf = key
  if(key.le.0) keyf = 1
  if(key.ge.7) keyf = 6
  neval = 0
  if(keyf.eq.1) call dqk15(f,a,b,result,abserr,defabs,resabs)
  if(keyf.eq.2) call dqk21(f,a,b,result,abserr,defabs,resabs)
  if(keyf.eq.3) call dqk31(f,a,b,result,abserr,defabs,resabs)
  if(keyf.eq.4) call dqk41(f,a,b,result,abserr,defabs,resabs)
  if(keyf.eq.5) call dqk51(f,a,b,result,abserr,defabs,resabs)
  if(keyf.eq.6) call dqk61(f,a,b,result,abserr,defabs,resabs)
  last = 1
  rlist(1) = result
  elist(1) = abserr
  iord(1) = 1
!
!  test on accuracy.
!
  errbnd =  max ( epsabs, epsrel* abs ( result ) )

  if(abserr.le.0.5D+02* epmach * defabs .and. &
    abserr.gt.errbnd) then
    ier = 2
  end if

  if(limit.eq.1) then
    ier = 1
  end if

  if ( ier .ne. 0 .or. &
    (abserr .le. errbnd .and. abserr .ne. resabs ) .or. &
    abserr .eq. 0.0D+00 ) then

    if(keyf.ne.1) then
      neval = (10*keyf+1)*(2*neval+1)
    else
      neval = 30*neval+15
    end if

    return

  end if
!
!  initialization
!
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
  do last = 2, limit
!
!  bisect the subinterval with the largest error estimate.
!
    a1 = alist(maxerr)
    b1 = 0.5D+00*(alist(maxerr)+blist(maxerr))
    a2 = b1
    b2 = blist(maxerr)

    if(keyf.eq.1) call dqk15(f,a1,b1,area1,error1,resabs,defab1)
    if(keyf.eq.2) call dqk21(f,a1,b1,area1,error1,resabs,defab1)
    if(keyf.eq.3) call dqk31(f,a1,b1,area1,error1,resabs,defab1)
    if(keyf.eq.4) call dqk41(f,a1,b1,area1,error1,resabs,defab1)
    if(keyf.eq.5) call dqk51(f,a1,b1,area1,error1,resabs,defab1)
    if(keyf.eq.6) call dqk61(f,a1,b1,area1,error1,resabs,defab1)

    if(keyf.eq.1) call dqk15(f,a2,b2,area2,error2,resabs,defab2)
    if(keyf.eq.2) call dqk21(f,a2,b2,area2,error2,resabs,defab2)
    if(keyf.eq.3) call dqk31(f,a2,b2,area2,error2,resabs,defab2)
    if(keyf.eq.4) call dqk41(f,a2,b2,area2,error2,resabs,defab2)
    if(keyf.eq.5) call dqk51(f,a2,b2,area2,error2,resabs,defab2)
    if(keyf.eq.6) call dqk61(f,a2,b2,area2,error2,resabs,defab2)
!
!  improve previous approximations to integral
!  and error and test for accuracy.
!
    neval = neval+1
    area12 = area1+area2
    erro12 = error1+error2
    errsum = errsum+erro12-errmax
    area = area+area12-rlist(maxerr)

    if ( defab1 .ne. error1 .and. defab2 .ne. error2 ) then

      if( abs ( rlist(maxerr)-area12).le.0.1D-04* abs ( area12) &
        .and. erro12.ge.0.99D+00*errmax) then
        iroff1 = iroff1+1
      end if

      if(last.gt.10.and.erro12.gt.errmax) then
        iroff2 = iroff2+1
      end if

    end if

    rlist(maxerr) = area1
    rlist(last) = area2
    errbnd =  max ( epsabs,epsrel* abs ( area))

    if ( errbnd .lt. errsum ) then
!
!  test for roundoff error and eventually set error flag.
!
      if(iroff1.ge.6.or.iroff2.ge.20) then
        ier = 2
      end if
!
!  set error flag in the case that the number of subintervals
!  equals limit.
!
      if(last.eq.limit) then
        ier = 1
      end if
!
!  set error flag in the case of bad integrand behaviour
!  at a point of the integration range.
!
      if( max (  abs ( a1), abs ( b2)).le.(0.1D+01+0.1D+03* &
        epmach)*( abs ( a2)+0.1D+04*uflow)) then
        ier = 3
      end if

    end if
!
!  append the newly-created intervals to the list.
!
    if(error2.le.error1) then
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
!  with the largest error estimate (to be bisected next).
!
    call dqpsrt(limit,last,maxerr,errmax,elist,iord,nrmax)

    if(ier.ne.0.or.errsum.le.errbnd) then
      exit
    end if

  end do
!
!  compute final result.
!
  result = 0.0D+00
  do k=1,last
    result = result+rlist(k)
  end do
  abserr = errsum

  if(keyf.ne.1) then
    neval = (10*keyf+1)*(2*neval+1)
  else
    neval = 30*neval+15
  end if

  return
end

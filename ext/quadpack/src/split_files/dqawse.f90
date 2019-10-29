subroutine dqawse(f,a,b,alfa,beta,integr,epsabs,epsrel,limit, &
     result,abserr,neval,ier,alist,blist,rlist,elist,iord,last)

!*****************************************************************************80
!
!! DQAWSE estimates integrals with algebraico-logarithmic end singularities.
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
!      definite integral i = integral of f*w over (a,b),
!      (where w shows a singular behaviour at the end points,
!      see parameter integr).
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
!               upper limit of integration, b.gt.a
!               if b.le.a, the routine will end with ier = 6.
!
!      alfa   - real ( kind = 8 )
!               parameter in the weight function, alfa.gt.(-1)
!               if alfa.le.(-1), the routine will end with
!               ier = 6.
!
!      beta   - real ( kind = 8 )
!               parameter in the weight function, beta.gt.(-1)
!               if beta.le.(-1), the routine will end with
!               ier = 6.
!
!      integr - integer ( kind = 4 )
!               indicates which weight function is to be used
!               = 1  (x-a)**alfa*(b-x)**beta
!               = 2  (x-a)**alfa*(b-x)**beta*log(x-a)
!               = 3  (x-a)**alfa*(b-x)**beta*log(b-x)
!               = 4  (x-a)**alfa*(b-x)**beta*log(x-a)*log(b-x)
!               if integr.lt.1 or integr.gt.4, the routine
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
!               gives an upper bound on the number of subintervals
!               in the partition of (a,b), limit.ge.2
!               if limit.lt.2, the routine will end with ier = 6.
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
!                       the estimates for the integral and error
!                       are less reliable. it is assumed that the
!                       requested accuracy has not been achieved.
!      error messages
!                   = 1 maximum number of subdivisions allowed
!                       has been achieved. one can allow more
!                       subdivisions by increasing the value of
!                       limit. however, if this yields no
!                       improvement, it is advised to analyze the
!                       integrand in order to determine the
!                       integration difficulties which prevent the
!                       requested tolerance from being achieved.
!                       in case of a jump discontinuity or a local
!                       singularity of algebraico-logarithmic type
!                       at one or more interior points of the
!                       integration range, one should proceed by
!                       splitting up the interval at these
!                       points and calling the integrator on the
!                       subranges.
!                   = 2 the occurrence of roundoff error is
!                       detected, which prevents the requested
!                       tolerance from being achieved.
!                   = 3 extremely bad integrand behaviour occurs
!                       at some points of the integration
!                       interval.
!                   = 6 the input is invalid, because
!                       b.le.a or alfa.le.(-1) or beta.le.(-1), or
!                       integr.lt.1 or integr.gt.4, or
!                       (epsabs.le.0 and
!                        epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
!                       or limit.lt.2.
!                       result, abserr, neval, rlist(1), elist(1),
!                       iord(1) and last are set to zero. alist(1)
!                       and blist(1) are set to a and b
!                       respectively.
!
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
!               vector of dimension at least limit,the first
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
!               of which are pointers to the error
!               estimates over the subintervals, so that
!               elist(iord(1)), ..., elist(iord(k)) with k = last
!               if last.le.(limit/2+2), and k = limit+1-last
!               otherwise form a decreasing sequence
!
!      last   - integer ( kind = 4 )
!               number of subintervals actually produced in
!               the subdivision process
!
!  Local parameters:
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
!      machine dependent constants
!
!     epmach is the largest relative spacing.
!     uflow is the smallest positive magnitude.
!
  implicit none

  real ( kind = 8 ) a,abserr,alfa,alist,area,area1,area12,area2,a1, &
    a2,b,beta,blist,b1,b2,centre,elist,epmach, &
    epsabs,epsrel,errbnd,errmax,error1,erro12,error2,errsum,f, &
    resas1,resas2,result,rg,rh,ri,rj,rlist,uflow
  integer ( kind = 4 ) ier,integr,iord,iroff1,iroff2,k,last,limit
  integer ( kind = 4 )maxerr
  integer ( kind = 4 ) nev
  integer ( kind = 4 ) neval,nrmax

  external f

  dimension alist(limit),blist(limit),rlist(limit),elist(limit), &
    iord(limit),ri(25),rj(25),rh(25),rg(25)

  epmach = epsilon ( epmach )
  uflow = tiny ( uflow )
!
!  test on validity of parameters
!
  neval = 0
  last = 0
  rlist(1) = 0.0D+00
  elist(1) = 0.0D+00
  iord(1) = 0
  result = 0.0D+00
  abserr = 0.0D+00

  if ( b.le.a .or. &
    (epsabs.eq.0.0D+00 .and. epsrel .lt. max ( 0.5D+02*epmach,0.5D-28) ) .or. &
    alfa .le. (-0.1D+01) .or. &
    beta .le. (-0.1D+01) .or. &
    integr.lt.1 .or. &
    integr.gt.4 .or. &
    limit.lt.2 ) then
    ier = 6
    return
  end if

  ier = 0
!
!  compute the modified chebyshev moments.
!
  call dqmomo(alfa,beta,ri,rj,rg,rh,integr)
!
!  integrate over the intervals (a,(a+b)/2) and ((a+b)/2,b).
!
  centre = 0.5D+00*(b+a)
  call dqc25s(f,a,b,a,centre,alfa,beta,ri,rj,rg,rh,area1, &
    error1,resas1,integr,nev)
  neval = nev
  call dqc25s(f,a,b,centre,b,alfa,beta,ri,rj,rg,rh,area2, &
    error2,resas2,integr,nev)
  last = 2
  neval = neval+nev
  result = area1+area2
  abserr = error1+error2
!
!  test on accuracy.
!
  errbnd = max ( epsabs,epsrel* abs ( result))
!
!  initialization
!
  if ( error2 .le. error1 ) then
    alist(1) = a
    alist(2) = centre
    blist(1) = centre
    blist(2) = b
    rlist(1) = area1
    rlist(2) = area2
    elist(1) = error1
    elist(2) = error2
  else
    alist(1) = centre
    alist(2) = a
    blist(1) = b
    blist(2) = centre
    rlist(1) = area2
    rlist(2) = area1
    elist(1) = error2
    elist(2) = error1
  end if

  iord(1) = 1
  iord(2) = 2
  if(limit.eq.2) ier = 1

  if(abserr.le.errbnd.or.ier.eq.1) then
    return
  end if

  errmax = elist(1)
  maxerr = 1
  nrmax = 1
  area = result
  errsum = abserr
  iroff1 = 0
  iroff2 = 0
!
!  main do-loop
!
  do 60 last = 3,limit
!
!  bisect the subinterval with largest error estimate.
!
    a1 = alist(maxerr)
    b1 = 0.5D+00*(alist(maxerr)+blist(maxerr))
    a2 = b1
    b2 = blist(maxerr)

    call dqc25s(f,a,b,a1,b1,alfa,beta,ri,rj,rg,rh,area1, &
    error1,resas1,integr,nev)
    neval = neval+nev
    call dqc25s(f,a,b,a2,b2,alfa,beta,ri,rj,rg,rh,area2, &
    error2,resas2,integr,nev)
    neval = neval+nev
!
!  improve previous approximations integral and error and test for accuracy.
!
    area12 = area1+area2
    erro12 = error1+error2
    errsum = errsum+erro12-errmax
    area = area+area12-rlist(maxerr)
    if(a.eq.a1.or.b.eq.b2) go to 30
    if(resas1.eq.error1.or.resas2.eq.error2) go to 30
!
!  test for roundoff error.
!
    if( abs ( rlist(maxerr)-area12).lt.0.1D-04* abs ( area12) &
    .and.erro12.ge.0.99D+00*errmax) iroff1 = iroff1+1
    if(last.gt.10.and.erro12.gt.errmax) iroff2 = iroff2+1
   30   rlist(maxerr) = area1
    rlist(last) = area2
!
!  test on accuracy.
!
    errbnd =  max ( epsabs,epsrel* abs ( area))
    if(errsum.le.errbnd) go to 35
!
!  set error flag in the case that the number of interval
!  bisections exceeds limit.
!
    if(last.eq.limit) ier = 1
!
!  set error flag in the case of roundoff error.
!
    if(iroff1.ge.6.or.iroff2.ge.20) ier = 2
!
!  set error flag in the case of bad integrand behaviour
!  at interior points of integration range.
!
    if( max (  abs ( a1), abs ( b2)).le.(0.1D+01+0.1D+03*epmach)* &
    ( abs ( a2)+0.1D+04*uflow)) ier = 3
!
!  append the newly-created intervals to the list.
!
   35   if(error2.gt.error1) go to 40
    alist(last) = a2
    blist(maxerr) = b1
    blist(last) = b2
    elist(maxerr) = error1
    elist(last) = error2
    go to 50

   40   alist(maxerr) = a2
    alist(last) = a1
    blist(last) = b1
    rlist(maxerr) = area2
    rlist(last) = area1
    elist(maxerr) = error2
    elist(last) = error1
!
!  call dqpsrt to maintain the descending ordering
!  in the list of error estimates and select the subinterval
!  with largest error estimate (to be bisected next).
!
   50   call dqpsrt(limit,last,maxerr,errmax,elist,iord,nrmax)
    if (ier.ne.0.or.errsum.le.errbnd) go to 70
   60 continue
!
!  compute final result.
!
   70 continue

  result = 0.0D+00
  do k=1,last
    result = result+rlist(k)
  end do

  abserr = errsum
  999 continue

  return
end

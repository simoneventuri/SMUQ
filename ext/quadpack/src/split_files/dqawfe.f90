subroutine dqawfe(f,a,omega,integr,epsabs,limlst,limit,maxp1, &
     result,abserr,neval,ier,rslst,erlst,ierlst,lst,alist,blist, &
     rlist,elist,iord,nnlog,chebmo)

!*****************************************************************************80
!
!! DQAWFE computes Fourier integrals.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  the routine calculates an approximation result to a
!      given fourier integal
!      i = integral of f(x)*w(x) over (a,infinity)
!      where w(x)=cos(omega*x) or w(x)=sin(omega*x),
!      hopefully satisfying following claim for accuracy
!      abs(i-result).le.epsabs.
!
!  Parameters:
!
!   on entry
!      f      - real ( kind = 8 )
!               function subprogram defining the integrand
!               function f(x). the actual name for f needs to
!               be declared e x t e r n a l in the driver program.
!
!      a      - real ( kind = 8 )
!               lower limit of integration
!
!      omega  - real ( kind = 8 )
!               parameter in the weight function
!
!      integr - integer ( kind = 4 )
!               indicates which weight function is used
!               integr = 1      w(x) = cos(omega*x)
!               integr = 2      w(x) = sin(omega*x)
!               if integr.ne.1.and.integr.ne.2, the routine will
!               end with ier = 6.
!
!      epsabs - real ( kind = 8 )
!               absolute accuracy requested, epsabs.gt.0
!               if epsabs.le.0, the routine will end with ier = 6.
!
!      limlst - integer ( kind = 4 )
!               limlst gives an upper bound on the number of
!               cycles, limlst.ge.1.
!               if limlst.lt.3, the routine will end with ier = 6.
!
!      limit  - integer ( kind = 4 )
!               gives an upper bound on the number of subintervals
!               allowed in the partition of each cycle, limit.ge.1
!               each cycle, limit.ge.1.
!
!      maxp1  - integer ( kind = 4 )
!               gives an upper bound on the number of
!               chebyshev moments which can be stored, i.e.
!               for the intervals of lengths abs(b-a)*2**(-l),
!               l=0,1, ..., maxp1-2, maxp1.ge.1
!
!   on return
!      result - real ( kind = 8 )
!               approximation to the integral x
!
!      abserr - real ( kind = 8 )
!               estimate of the modulus of the absolute error,
!               which should equal or exceed abs(i-result)
!
!      neval  - integer ( kind = 4 )
!               number of integrand evaluations
!
!      ier    - ier = 0 normal and reliable termination of
!                       the routine. it is assumed that the
!                       requested accuracy has been achieved.
!               ier.gt.0 abnormal termination of the routine. the
!                       estimates for integral and error are less
!                       reliable. it is assumed that the requested
!                       accuracy has not been achieved.
!      error messages
!              if omega.ne.0
!               ier = 1 maximum number of  cycles  allowed
!                       has been achieved., i.e. of subintervals
!                       (a+(k-1)c,a+kc) where
!                       c = (2*int(abs(omega))+1)*pi/abs(omega),
!                       for k = 1, 2, ..., lst.
!                       one can allow more cycles by increasing
!                       the value of limlst (and taking the
!                       according dimension adjustments into
!                       account).
!                       examine the array iwork which contains
!                       the error flags on the cycles, in order to
!                       look for eventual local integration
!                       difficulties. if the position of a local
!                       difficulty can be determined (e.g.
!                       singularity, discontinuity within the
!                       interval) one will probably gain from
!                       splitting up the interval at this point
!                       and calling appropriate integrators on
!                       the subranges.
!                   = 4 the extrapolation table constructed for
!                       convergence acceleration of the series
!                       formed by the integral contributions over
!                       the cycles, does not converge to within
!                       the requested accuracy. as in the case of
!                       ier = 1, it is advised to examine the
!                       array iwork which contains the error
!                       flags on the cycles.
!                   = 6 the input is invalid because
!                       (integr.ne.1 and integr.ne.2) or
!                        epsabs.le.0 or limlst.lt.3.
!                        result, abserr, neval, lst are set
!                        to zero.
!                   = 7 bad integrand behaviour occurs within one
!                       or more of the cycles. location and type
!                       of the difficulty involved can be
!                       determined from the vector ierlst. here
!                       lst is the number of cycles actually
!                       needed (see below).
!                       ierlst(k) = 1 the maximum number of
!                                     subdivisions (= limit) has
!                                     been achieved on the k th
!                                     cycle.
!                                 = 2 occurrence of roundoff error
!                                     is detected and prevents the
!                                     tolerance imposed on the
!                                     k th cycle, from being
!                                     achieved.
!                                 = 3 extremely bad integrand
!                                     behaviour occurs at some
!                                     points of the k th cycle.
!                                 = 4 the integration procedure
!                                     over the k th cycle does
!                                     not converge (to within the
!                                     required accuracy) due to
!                                     roundoff in the
!                                     extrapolation procedure
!                                     invoked on this cycle. it
!                                     is assumed that the result
!                                     on this interval is the
!                                     best which can be obtained.
!                                 = 5 the integral over the k th
!                                     cycle is probably divergent
!                                     or slowly convergent. it
!                                     must be noted that
!                                     divergence can occur with
!                                     any other value of
!                                     ierlst(k).
!              if omega = 0 and integr = 1,
!              the integral is calculated by means of dqagie
!              and ier = ierlst(1) (with meaning as described
!              for ierlst(k), k = 1).
!
!      rslst  - real ( kind = 8 )
!               vector of dimension at least limlst
!               rslst(k) contains the integral contribution
!               over the interval (a+(k-1)c,a+kc) where
!               c = (2*int(abs(omega))+1)*pi/abs(omega),
!               k = 1, 2, ..., lst.
!               note that, if omega = 0, rslst(1) contains
!               the value of the integral over (a,infinity).
!
!      erlst  - real ( kind = 8 )
!               vector of dimension at least limlst
!               erlst(k) contains the error estimate corresponding
!               with rslst(k).
!
!      ierlst - integer ( kind = 4 )
!               vector of dimension at least limlst
!               ierlst(k) contains the error flag corresponding
!               with rslst(k). for the meaning of the local error
!               flags see description of output parameter ier.
!
!      lst    - integer ( kind = 4 )
!               number of subintervals needed for the integration
!               if omega = 0 then lst is set to 1.
!
!      alist, blist, rlist, elist - real ( kind = 8 )
!               vector of dimension at least limit,
!
!      iord, nnlog - integer ( kind = 4 )
!               vector of dimension at least limit, providing
!               space for the quantities needed in the subdivision
!               process of each cycle
!
!      chebmo - real ( kind = 8 )
!               array of dimension at least (maxp1,25), providing
!               space for the chebyshev moments needed within the
!               cycles
!
!  Local Parameters:
!
!      the dimension of  psum  is determined by the value of
!      limexp in routine dqelg (psum must be of dimension
!      (limexp+2) at least).
!
!     c1, c2    - end points of subinterval (of length cycle)
!     cycle     - (2*int(abs(omega))+1)*pi/abs(omega)
!     psum      - vector of dimension at least (limexp+2)
!                 (see routine dqelg)
!                 psum contains the part of the epsilon table
!                 which is still needed for further computations.
!                 each element of psum is a partial sum of the
!                 series which should sum to the value of the
!                 integral.
!     errsum    - sum of error estimates over the subintervals,
!                 calculated cumulatively
!     epsa      - absolute tolerance requested over current
!                 subinterval
!     chebmo    - array containing the modified chebyshev
!                 moments (see also routine dqc25f)
!
  implicit none

  real ( kind = 8 ) a,abseps,abserr,alist,blist,chebmo,correc,cycle, &
    c1,c2,dl,dla,drl,elist,erlst,ep,eps,epsa, &
    epsabs,errsum,f,fact,omega,p,pi,p1,psum,reseps,result,res3la, &
    rlist,rslst,uflow
  integer ( kind = 4 ) ier,ierlst,integr,iord,ktmin,l,last,lst,limit
  integer ( kind = 4 ) limlst
  integer ( kind = 4 ) ll
  integer ( kind = 4 ) maxp1,momcom,nev,neval,nnlog,nres,numrl2

  dimension alist(limit),blist(limit),chebmo(maxp1,25),elist(limit), &
    erlst(limlst),ierlst(limlst),iord(limit),nnlog(limit),psum(52), &
    res3la(3),rlist(limit),rslst(limlst)

  external f

  data p / 0.9D+00 /
  data pi / 3.14159265358979323846264338327950D+00 /
!
!  test on validity of parameters
!
  result = 0.0D+00
  abserr = 0.0D+00
  neval = 0
  lst = 0
  ier = 0

  if((integr.ne.1.and.integr.ne.2).or.epsabs.le.0.0D+00.or. &
    limlst.lt.3) then
    ier = 6
    return
  end if

  if(omega.ne.0.0D+00) go to 10
!
!  integration by dqagie if omega is zero
!
  if(integr.eq.1) then
    call dqagie(f,0.0D+00,1,epsabs,0.0D+00,limit, &
      result,abserr,neval,ier,alist,blist,rlist,elist,iord,last)
  end if

  rslst(1) = result
  erlst(1) = abserr
  ierlst(1) = ier
  lst = 1
  go to 999
!
!  initializations
!
   10 l =  abs ( omega)
  dl = 2*l+1
  cycle = dl*pi/ abs ( omega)
  ier = 0
  ktmin = 0
  neval = 0
  numrl2 = 0
  nres = 0
  c1 = a
  c2 = cycle+a
  p1 = 0.1D+01-p
  uflow = tiny ( uflow )
  eps = epsabs
  if(epsabs.gt.uflow/p1) eps = epsabs*p1
  ep = eps
  fact = 0.1D+01
  correc = 0.0D+00
  abserr = 0.0D+00
  errsum = 0.0D+00
!
!  main do-loop
!
  do lst = 1,limlst
!
!  integrate over current subinterval.
!
    dla = lst
    epsa = eps*fact
    call dqawoe(f,c1,c2,omega,integr,epsa,0.0D+00,limit,lst,maxp1, &
    rslst(lst),erlst(lst),nev,ierlst(lst),last,alist,blist,rlist, &
    elist,iord,nnlog,momcom,chebmo)
    neval = neval+nev
    fact = fact*p
    errsum = errsum+erlst(lst)
    drl = 0.5D+02* abs ( rslst(lst))
!
!  test on accuracy with partial sum
!
    if((errsum+drl).le.epsabs.and.lst.ge.6) go to 80
    correc =  max ( correc,erlst(lst))
    if(ierlst(lst).ne.0) eps =  max ( ep,correc*p1)
    if(ierlst(lst).ne.0) ier = 7
    if(ier.eq.7.and.(errsum+drl).le.correc*0.1D+02.and. &
    lst.gt.5) go to 80
    numrl2 = numrl2+1
    if(lst.gt.1) go to 20
    psum(1) = rslst(1)
    go to 40
   20   psum(numrl2) = psum(ll)+rslst(lst)
    if(lst.eq.2) go to 40
!
!  test on maximum number of subintervals
!
    if(lst.eq.limlst) ier = 1
!
!  perform new extrapolation
!
    call dqelg(numrl2,psum,reseps,abseps,res3la,nres)
!
!  test whether extrapolated result is influenced by roundoff
!
    ktmin = ktmin+1
    if(ktmin.ge.15.and.abserr.le.0.1D-02*(errsum+drl)) ier = 4
    if(abseps.gt.abserr.and.lst.ne.3) go to 30
    abserr = abseps
    result = reseps
    ktmin = 0
!
!  if ier is not 0, check whether direct result (partial sum)
!  or extrapolated result yields the best integral
!  approximation
!
    if((abserr+0.1D+02*correc).le.epsabs.or. &
    (abserr.le.epsabs.and.0.1D+02*correc.ge.epsabs)) go to 60
   30   if(ier.ne.0.and.ier.ne.7) go to 60
   40   ll = numrl2
    c1 = c2
    c2 = c2+cycle
  end do
!
!  set final result and error estimate
!
   60 abserr = abserr+0.1D+02*correc
  if(ier.eq.0) go to 999
  if(result.ne.0.0D+00.and.psum(numrl2).ne.0.0D+00) go to 70
  if(abserr.gt.errsum) go to 80
  if(psum(numrl2).eq.0.0D+00) go to 999
   70 if(abserr/ abs ( result).gt.(errsum+drl)/ abs ( psum(numrl2))) &
    go to 80
  if(ier.ge.1.and.ier.ne.7) abserr = abserr+drl
  go to 999
   80 result = psum(numrl2)
  abserr = errsum+drl
  999 continue

  return
end

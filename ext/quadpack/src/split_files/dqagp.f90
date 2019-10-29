subroutine dqagp ( f, a, b, npts2, points, epsabs, epsrel, result, abserr, &
  neval, ier, leniw, lenw, last, iwork, work )

!*****************************************************************************80
!
!! DQAGP computes a definite integral.
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
!      definite integral i = integral of f over (a,b),
!      hopefully satisfying following claim for accuracy
!      break points of the integration interval, where local
!      difficulties of the integrand may occur (e.g.
!      singularities, discontinuities), are provided by the user.
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
!               range, npts.ge.2.
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
!                       extrapolation table.
!                       it is presumed that the requested
!                       tolerance cannot be achieved, and that
!                       the returned result is the best which
!                       can be obtained.
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
!                       result, abserr, neval, last are set to
!                       zero. exept when leniw or lenw or npts2 is
!                       invalid, iwork(1), iwork(limit+1),
!                       work(limit*2+1) and work(limit*3+1)
!                       are set to zero.
!                       work(1) is set to a and work(limit+1)
!                       to b (where limit = (leniw-npts2)/2).
!
!   dimensioning parameters
!      leniw - integer ( kind = 4 )
!              dimensioning parameter for iwork
!              leniw determines limit = (leniw-npts2)/2,
!              which is the maximum number of subintervals in the
!              partition of the given integration interval (a,b),
!              leniw.ge.(3*npts2-2).
!              if leniw.lt.(3*npts2-2), the routine will end with
!              ier = 6.
!
!      lenw  - integer ( kind = 4 )
!              dimensioning parameter for work
!              lenw must be at least leniw*2-npts2.
!              if lenw.lt.leniw*2-npts2, the routine will end
!              with ier = 6.
!
!      last  - integer ( kind = 4 )
!              on return, last equals the number of subintervals
!              produced in the subdivision process, which
!              determines the number of significant elements
!              actually in the work arrays.
!
!   work arrays
!      iwork - integer ( kind = 4 )
!              vector of dimension at least leniw. on return,
!              the first k elements of which contain
!              pointers to the error estimates over the
!              subintervals, such that work(limit*3+iwork(1)),...,
!              work(limit*3+iwork(k)) form a decreasing
!              sequence, with k = last if last.le.(limit/2+2), and
!              k = limit+1-last otherwise
!              iwork(limit+1), ...,iwork(limit+last) contain the
!               subdivision levels of the subintervals, i.e.
!               if (aa,bb) is a subinterval of (p1,p2)
!               where p1 as well as p2 is a user-provided
!               break point or integration limit, then (aa,bb) has
!               level l if abs(bb-aa) = abs(p2-p1)*2**(-l),
!              iwork(limit*2+1), ..., iwork(limit*2+npts2) have
!               no significance for the user,
!              note that limit = (leniw-npts2)/2.
!
!      work  - real ( kind = 8 )
!              vector of dimension at least lenw
!              on return
!              work(1), ..., work(last) contain the left
!               end points of the subintervals in the
!               partition of (a,b),
!              work(limit+1), ..., work(limit+last) contain
!               the right end points,
!              work(limit*2+1), ..., work(limit*2+last) contain
!               the integral approximations over the subintervals,
!              work(limit*3+1), ..., work(limit*3+last)
!               contain the corresponding error estimates,
!              work(limit*4+1), ..., work(limit*4+npts2)
!               contain the integration limits and the
!               break points sorted in an ascending sequence.
!              note that limit = (leniw-npts2)/2.
!
  implicit none

  real ( kind = 8 ) a,abserr,b,epsabs,epsrel,f,points,result,work
  integer ( kind = 4 ) ier,iwork,last,leniw,lenw,limit,lvl,l1,l2,l3, &
    l4,neval,npts2

  dimension iwork(leniw),points(npts2),work(lenw)

  external f
!
!  check validity of limit and lenw.
!
  ier = 6
  neval = 0
  last = 0
  result = 0.0D+00
  abserr = 0.0D+00
  if(leniw.lt.(3*npts2-2).or.lenw.lt.(leniw*2-npts2).or.npts2.lt.2) &
    go to 10
!
!  prepare call for dqagpe.
!
  limit = (leniw-npts2)/2
  l1 = limit+1
  l2 = limit+l1
  l3 = limit+l2
  l4 = limit+l3

  call dqagpe(f,a,b,npts2,points,epsabs,epsrel,limit,result,abserr, &
    neval,ier,work(1),work(l1),work(l2),work(l3),work(l4), &
    iwork(1),iwork(l1),iwork(l2),last)
!
!  call error handler if necessary.
!
  lvl = 0
10    if(ier.eq.6) lvl = 1

  if(ier.ne.0) then
    call xerror('abnormal return from dqagp',26,ier,lvl)
  end if

  return
end

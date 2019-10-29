subroutine dqpsrt ( limit, last, maxerr, ermax, elist, iord, nrmax )

!*****************************************************************************80
!
!! DQPSRT maintains the order of a list of local error estimates.
!
!  Modified:
!
!    11 September 2015
!
!  Author:
!
!    Robert Piessens, Elise de Doncker
!
!***purpose  this routine maintains the descending ordering in the
!      list of the local error estimated resulting from the
!      interval subdivision process. at each call two error
!      estimates are inserted using the sequential search
!      method, top-down for the largest error estimate and
!      bottom-up for the smallest error estimate.
!
!  Parameters:
!
!        limit  - integer ( kind = 4 )
!                 maximum number of error estimates the list
!                 can contain
!
!        last   - integer ( kind = 4 )
!                 number of error estimates currently in the list
!
!        maxerr - integer ( kind = 4 )
!                 maxerr points to the nrmax-th largest error
!                 estimate currently in the list
!
!        ermax  - real ( kind = 8 )
!                 nrmax-th largest error estimate
!                 ermax = elist(maxerr)
!
!        elist  - real ( kind = 8 )
!                 vector of dimension last containing
!                 the error estimates
!
!        iord   - integer ( kind = 4 )
!                 vector of dimension last, the first k elements
!                 of which contain pointers to the error
!                 estimates, such that
!                 elist(iord(1)),...,  elist(iord(k))
!                 form a decreasing sequence, with
!                 k = last if last.le.(limit/2+2), and
!                 k = limit+1-last otherwise
!
!        nrmax  - integer ( kind = 4 )
!                 maxerr = iord(nrmax)
!
  implicit none

  real ( kind = 8 ) elist,ermax,errmax,errmin
  integer ( kind = 4 ) i,ibeg,ido,iord,isucc,j,jbnd,jupbn,k,last, &
    lim
  integer ( kind = 4 ) limit
  integer ( kind = 4 ) maxerr
  integer ( kind = 4 ) nrmax
  dimension elist(last),iord(last)
!
!  check whether the list contains more than
!  two error estimates.
!
  if(last.gt.2) go to 10
  iord(1) = 1
  iord(2) = 2
  go to 90
!
!  this part of the routine is only executed if, due to a
!  difficult integrand, subdivision increased the error
!  estimate. in the normal case the insert procedure should
!  start after the nrmax-th largest error estimate.
!
   10 errmax = elist(maxerr)

  ido = nrmax-1
  do i = 1,ido
    isucc = iord(nrmax-1)
    if(errmax.le.elist(isucc)) go to 30
    iord(nrmax) = isucc
    nrmax = nrmax-1
  end do
!
!  compute the number of elements in the list to be maintained
!  in descending order. this number depends on the number of
!  subdivisions still allowed.
!
   30 jupbn = last
  if(last.gt.(limit/2+2)) jupbn = limit+3-last
  errmin = elist(last)
!
!  insert errmax by traversing the list top-down,
!  starting comparison from the element elist(iord(nrmax+1)).
!
  jbnd = jupbn-1
  ibeg = nrmax+1

  do i=ibeg,jbnd
    isucc = iord(i)
    if(errmax.ge.elist(isucc)) go to 60
    iord(i-1) = isucc
  end do

  iord(jbnd) = maxerr
  iord(jupbn) = last
  go to 90
!
!  insert errmin by traversing the list bottom-up.
!
   60 iord(i-1) = maxerr
  k = jbnd

  do j=i,jbnd
    isucc = iord(k)
    if(errmin.lt.elist(isucc)) go to 80
    iord(k+1) = isucc
    k = k-1
  end do

  iord(i) = last
  go to 90
   80 iord(k+1) = last
!
!     set maxerr and ermax.
!
   90 maxerr = iord(nrmax)
  ermax = elist(maxerr)

  return
end

subroutine cdffnc ( which, p, q, f, dfn, dfd, pnonc, status, bound )

!*****************************************************************************80
!
!! CDFFNC evaluates the CDF of the Noncentral F distribution.
!
!  Discussion:
!
!    This routine originally used 1.0D+300 as the upper bound for the
!    interval in which many of the missing parameters are to be sought.
!    Since the underlying rootfinder routine needs to evaluate the
!    function at this point, it is no surprise that the program was
!    experiencing overflows.  A less extravagant upper bound
!    is being tried for now!
!
!    This routine calculates any one parameter of the Noncentral F distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The computation time required for this routine is proportional
!    to the noncentrality parameter PNONC.  Very large values of
!    this parameter can consume immense computer resources.  This is
!    why the search range is bounded by 10,000.
!
!    The value of the cumulative noncentral F distribution is not
!    necessarily monotone in either degree of freedom.  There thus
!    may be two values that provide a given CDF value.  This routine
!    assumes monotonicity and will find an arbitrary one of the two
!    values.
!
!    The CDF of the noncentral F distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF [ NoncentralFRatioDistribution [ DFN, DFD, PNONC ], X ]
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.6.20.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from F, DFN, DFD and PNONC;
!    2: Calculate F from P, Q, DFN, DFD and PNONC;
!    3: Calculate DFN from P, Q, F, DFD and PNONC;
!    4: Calculate DFD from P, Q, F, DFN and PNONC;
!    5: Calculate PNONC from P, Q, F, DFN and DFD.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to F of
!    the noncentral F-density.  If P is an input value it should
!    lie in the range [0,1) (Not including 1!).
!
!    Dummy, real ( kind = 8 ) Q, is not used by this subroutine,
!    and is only included for similarity with the other routines.
!    Its input value is not checked.  If P is to be computed, the
!    Q is set to 1 - P.
!
!    Input/output, real ( kind = 8 ) F, the upper limit of integration
!    of the noncentral F-density.  If this is an input value, it should
!    lie in the range: [0, +infinity).  If it is an output value, it
!    will be searched for in the range: [0,1.0D+30].
!
!    Input/output, real ( kind = 8 ) DFN, the number of degrees of freedom
!    of the numerator sum of squares.  If this is an input value, it should
!    lie in the range: (0, +infinity).  If it is an output value, it will
!    be searched for in the range: [ 1.0, 1.0D+30].
!
!    Input/output, real ( kind = 8 ) DFD, the number of degrees of freedom
!    of the denominator sum of squares.  If this is an input value, it should
!    be in range: (0, +infinity).  If it is an output value, it will be
!    searched for in the range [1.0, 1.0D+30].
!
!    Input/output, real ( kind = 8 ) PNONC, the noncentrality parameter
!    If this is an input value, it should be nonnegative.
!    If it is an output value, it will be searched for in the range: [0,1.0D+4].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
  implicit none

  real ( kind = 8 ), parameter :: atol = 1.0D-10
  real ( kind = 8 ) bound
  real ( kind = 8 ) ccum
  real ( kind = 8 ) cum
  real ( kind = 8 ) dfd
  real ( kind = 8 ) dfn
  real ( kind = 8 ) f
  real ( kind = 8 ) fx
  real ( kind = 8 ), parameter :: inf = 1.0D+30
  real ( kind = 8 ) p
  real ( kind = 8 ) pnonc
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  integer ( kind = 4 ) status
  real ( kind = 8 ), parameter :: tent4 = 1.0D+04
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) which

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFFNC - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 5.'
    return
  end if

  if ( 5 < which ) then
    bound = 5.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFFNC - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 5.'
    return
  end if
!
!  Unless P is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( p < 0.0D+00 ) then
      status = -2
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless F is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( f < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter F is out of range.'
      return
    end if
  end if
!
!  Unless DFN is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( dfn <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DFN is out of range.'
      return
    end if
  end if
!
!  Unless DFD is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( dfd <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter DFD is out of range.'
      return
    end if
  end if
!
!  Unless PNONC is to be computed, make sure it is legal.
!
  if ( which /= 5 ) then
    if ( pnonc < 0.0D+00 ) then
      bound = 0.0D+00
      status = -7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFFNC - Fatal error!'
      write ( *, '(a)' ) '  Input parameter PNONC is out of range.'
      return
    end if
  end if
!
! Calculate P and Q.
!
  if ( which == 1 ) then

    call cumfnc ( f, dfn, dfd, pnonc, p, q )
    status = 0
!
!  Calculate F.
!
  else if ( which == 2 ) then

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    f = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, f, fx, qleft, qhi )

    do while ( status == 1 )

      call cumfnc ( f, dfn, dfd, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, f, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
        return
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
        return
      end if
    end if
!
!  Calculate DFN.
!
  else if ( which == 3 ) then

    call dstinv ( 1.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    dfn = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, dfn, fx, qleft, qhi )

    do while ( status == 1 )

      call cumfnc ( f, dfn, dfd, pnonc, cum, ccum )
      fx = cum - p

      call dinvr ( status, dfn, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate DFD.
!
  else if ( which == 4 ) then

    call dstinv ( 1.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    dfd = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, dfd, fx, qleft, qhi )

    do while ( status == 1 )

      call cumfnc ( f, dfn, dfd, pnonc, cum, ccum )
      fx = cum - p
      call dinvr ( status, dfd, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate PNONC.
!
  else if ( which == 5 ) then

    call dstinv ( 0.0D+00, tent4, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    pnonc = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, pnonc, fx, qleft, qhi )

    do while ( status == 1 )

      call cumfnc ( f, dfn, dfd, pnonc, cum, ccum )
      fx = cum - p

      call dinvr ( status, pnonc, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = tent4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFFNC - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if

  end if

  return
end

subroutine dinvr ( status, x, fx, qleft, qhi )

!*****************************************************************************80
!
!! DINVR bounds the zero of the function and invokes DZROR.
!
!  Discussion:
!
!    This routine seeks to find bounds on a root of the function and
!    invokes DZROR to perform the zero finding.  DSTINV must have been
!    called before this routine in order to set its parameters.
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) STATUS.  At the beginning of a zero
!    finding problem, STATUS should be set to 0 and this routine invoked.
!    The value of parameters other than X will be ignored on this call.
!    If this routine needs the function to be evaluated, it will set STATUS
!    to 1 and return.  The value of the function should be set in FX and
!    this routine again called without changing any of its other parameters.
!    If this routine finishes without error, it returns with STATUS 0,
!    and X an approximate root of F(X).
!    If this routine cannot bound the function, it returns a negative STATUS and
!    sets QLEFT and QHI.
!
!    Output, real ( kind = 8 ) X, the value at which F(X) is to be evaluated.
!
!    Input, real ( kind = 8 ) FX, the value of F(X) calculated by the user
!    on the previous call, when this routine returned with STATUS = 1.
!
!    Output, logical QLEFT, is defined only if QMFINV returns FALSE.  In that
!    case, QLEFT is TRUE if the stepping search terminated unsucessfully
!    at SMALL, and FALSE if the search terminated unsucessfully at BIG.
!
!    Output, logical QHI, is defined only if QMFINV returns FALSE.  In that
!    case, it is TRUE if Y < F(X) at the termination of the search and FALSE
!    if F(X) < Y.
!
  implicit none

  real ( kind = 8 ) :: absstp
  real ( kind = 8 ) :: abstol
  real ( kind = 8 ) :: big
  real ( kind = 8 ) fbig
  real ( kind = 8 ) fsmall
  real ( kind = 8 ) fx
  integer ( kind = 4 ) i99999
  logical qbdd
  logical qcond
  logical qdum1
  logical qdum2
  logical qhi
  logical qincr
  logical qleft
  logical qlim
  logical qup
  real ( kind = 8 ) :: relstp
  real ( kind = 8 ) :: reltol
  real ( kind = 8 ) :: small
  integer ( kind = 4 ) status
  real ( kind = 8 ) step
  real ( kind = 8 ) :: stpmul
  real ( kind = 8 ) x
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlb
  real ( kind = 8 ) xlo
  real ( kind = 8 ) xsave
  real ( kind = 8 ) xub
  real ( kind = 8 ) yy
  real ( kind = 8 ) zabsst
  real ( kind = 8 ) zabsto
  real ( kind = 8 ) zbig
  real ( kind = 8 ) zrelst
  real ( kind = 8 ) zrelto
  real ( kind = 8 ) zsmall
  real ( kind = 8 ) zstpmu

  save

  if ( 0 < status ) then
    go to i99999
  end if

  qcond = .not. ( small <= x .and. x <= big )

  if ( .not. ( small <= x .and. x <= big ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DINVR - Fatal error!'
    write ( *, '(a)' ) '  The values SMALL, X, BIG are not monotone.'
    stop
  end if

  xsave = x
!
!  See that SMALL and BIG bound the zero and set QINCR.
!
  x = small
!
!  GET-function-VALUE
!
  assign 10 to i99999
  status = 1
  return

   10 continue

  fsmall = fx
  x = big
!
!  GET-function-VALUE
!
  assign 20 to i99999
  status = 1
  return

   20 continue

  fbig = fx

  qincr = ( fsmall < fbig )

  if ( fsmall <= fbig ) then

    if ( 0.0D+00 < fsmall ) then
      status = -1
      qleft = .true.
      qhi = .true.
      return
    end if

    if ( fbig < 0.0D+00 ) then
      status = -1
      qleft = .false.
      qhi = .false.
      return
    end if

  else if ( fbig < fsmall ) then

    if ( fsmall < 0.0D+00 ) then
      status = -1
      qleft = .true.
      qhi = .false.
      return
    end if

    if ( 0.0D+00 < fbig ) then
      status = -1
      qleft = .false.
      qhi = .true.
      return
    end if

  end if

  x = xsave
  step = max ( absstp, relstp * abs ( x ) )
!
!  YY = F(X) - Y
!  GET-function-VALUE
!
  assign 90 to i99999
  status = 1
  return

   90 continue

  yy = fx

  if ( yy == 0.0D+00 ) then
    status = 0
    return
  end if

  100 continue

  qup = ( qincr .and. ( yy < 0.0D+00 ) ) .or. &
        ( .not. qincr .and. ( 0.0D+00 < yy ) )
!
!  Handle case in which we must step higher.
!
  if (.not. qup ) then
    go to 170
  end if

  xlb = xsave
  xub = min ( xlb + step, big )
  go to 120

  110 continue

  if ( qcond ) then
    go to 150
  end if
!
!  YY = F(XUB) - Y
!
  120 continue

  x = xub
!
!  GET-function-VALUE
!
  assign 130 to i99999
  status = 1
  return

  130 continue

  yy = fx
  qbdd = ( qincr .and. ( 0.0D+00 <= yy ) ) .or. &
    ( .not. qincr .and. ( yy <= 0.0D+00 ) )
  qlim = ( big <= xub )
  qcond = qbdd .or. qlim

  if ( .not. qcond ) then
    step = stpmul * step
    xlb = xub
    xub = min ( xlb + step, big )
  end if

  go to 110

  150 continue

  if ( qlim .and. .not. qbdd ) then
    status = -1
    qleft = .false.
    qhi = .not. qincr
    x = big
    return
  end if

  160 continue

  go to 240
!
!  Handle the case in which we must step lower.
!
  170 continue

  xub = xsave
  xlb = max ( xub - step, small )
  go to 190

  180 continue

  if ( qcond ) then
    go to 220
  end if
!
!  YY = F(XLB) - Y
!
  190 continue

  x = xlb
!
!  GET-function-VALUE
!
  assign 200 to i99999
  status = 1
  return

  200 continue

  yy = fx
  qbdd = ( qincr .and. ( yy <= 0.0D+00 ) ) .or. &
    ( .not. qincr .and. ( 0.0D+00 <= yy ) )
  qlim = xlb <= small
  qcond = qbdd .or. qlim

  if ( .not. qcond ) then
    step = stpmul * step
    xub = xlb
    xlb = max ( xub - step, small )
  end if

  go to 180

  220 continue

  if ( qlim .and. ( .not. qbdd ) ) then
    status = -1
    qleft = .true.
    qhi = qincr
    x = small
    return
  end if

  230 continue
  240 continue

  call dstzr ( xlb, xub, abstol, reltol )
!
!  If we reach here, XLB and XUB bound the zero of F.
!
  status = 0
  go to 260

  250 continue

    if ( status /= 1 ) then
      x = xlo
      status = 0
      return
    end if

  260 continue

  call dzror ( status, x, fx, xlo, xhi, qdum1, qdum2 )

  if ( status /= 1 ) then
    go to 250
  end if
!
!  GET-function-VALUE
!
  assign 270 to i99999
  status = 1
  return

  270 continue
  go to 250

entry dstinv ( zsmall, zbig, zabsst, zrelst, zstpmu, zabsto, zrelto )

!*****************************************************************************80
!
!! DSTINV SeT INverse finder - Reverse Communication
!
!  Discussion:
!
!    This routine is given a monotone function F, and a value Y,
!    and seeks an argument value X such that F(X) = Y.
!
!    This routine uses reverse communication -- see DINVR.
!    This routine sets quantities needed by DINVR.
!
!    F must be a monotone function, the results of QMFINV are
!    otherwise undefined.  QINCR must be TRUE if F is nondecreasing
!    and FALSE if F is nonincreasing.
!
!    QMFINV will return TRUE if and only if F(SMALL) and
!    F(BIG) bracket Y, i. e.,
!      QINCR is TRUE and F(SMALL) <= Y <= F(BIG) or
!      QINCR is FALSE and F(BIG) <= Y <= F(SMALL)
!
!    If QMFINV returns TRUE, then the X returned satisfies
!    the following condition.  Let
!      TOL(X) = MAX ( ABSTOL, RELTOL * ABS ( X ) )
!    then if QINCR is TRUE,
!      F(X-TOL(X)) <= Y <= F(X+TOL(X))
!    and if QINCR is FALSE
!      F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
!
!    Compares F(X) with Y for the input value of X then uses QINCR
!    to determine whether to step left or right to bound the
!    desired X.  The initial step size is
!
!      max ( ABSSTP, RELSTP * ABS ( S ) )
!
!    for the input value of X.
!
!    Iteratively steps right or left until it bounds X.
!    At each step which doesn't bound X, the step size is doubled.
!    The routine is careful never to step beyond SMALL or BIG.  If
!    it hasn't bounded X at SMALL or BIG, QMFINV returns FALSE
!    after setting QLEFT and QHI.
!
!    If X is successfully bounded then Algorithm R of the paper
!    Bus and Dekker is employed to find the zero of the function F(X)-Y.
!    This is routine QRZERO.
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ZSMALL, ZBIG, the left and right endpoints
!    of the interval to be searched for a solution.
!
!    Input, real ( kind = 8 ) ZABSST, ZRELSTP, the initial step size in
!    the search is max ( ZABSST, ZRELST * abs ( X ) ).
!
!    Input, real ( kind = 8 ) STPMUL.  When a step doesn't bound the zero,
!    the stepsize is multiplied by STPMUL and another step taken.  A
!    popular value is 2.0.
!
!    Input, real ( kind = 8 ) ABSTOL, RELTOL, two numbers that determine
!    the accuracy of the solution
!
  small = zsmall
  big = zbig
  absstp = zabsst
  relstp = zrelst
  stpmul = zstpmu
  abstol = zabsto
  reltol = zrelto

  return
end

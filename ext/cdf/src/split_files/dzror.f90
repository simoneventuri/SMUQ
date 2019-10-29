subroutine dzror ( status, x, fx, xlo, xhi, qleft, qhi )

!*****************************************************************************80
!
!! DZROR seeks a zero of a function, using reverse communication.
!
!  Discussion:
!
!    This routine performs the zero finding.  STZROR must have been called
!    before this routine in order to set its parameters.
!
!  Modified:
!
!    09 June 2004
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
!    finding problem, STATUS should be set to 0 and ZROR invoked.  The value
!    of other parameters will be ignored on this call.
!    When ZROR needs the function evaluated, it will set
!    STATUS to 1 and return.  The value of the function
!    should be set in FX and ZROR again called without
!    changing any of its other parameters.
!    When ZROR has finished without error, it will return
!    with STATUS 0.  In that case (XLO,XHI) bound the answe
!    If ZROR finds an error (which implies that F(XLO)-Y an
!    F(XHI)-Y have the same sign, it returns STATUS -1.  In
!    this case, XLO and XHI are undefined.
!
!    Output, real ( kind = 8 ) X, the value of X at which F(X) is to
!    be evaluated.
!
!    Input, real ( kind = 8 ) FX, the value of F(X), which must be calculated
!    by the user when ZROR has returned on the previous call with STATUS = 1.
!
!    Output, real ( kind = 8 ) XLO, XHI, are lower and upper bounds for the
!    solution when ZROR returns with STATUS = 0.
!
!    Output, logical QLEFT,is TRUE if the stepping search terminated
!    unsucessfully at XLO.  If it is FALSE, the search terminated
!    unsucessfully at XHI.
!
!    Output, logical QHI, is TRUE if Y < F(X) at the termination of the
!    search and FALSE if F(X) < Y at the termination of the search.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) abstol
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  integer ( kind = 4 ) ext
  real ( kind = 8 ) fa
  real ( kind = 8 ) fb
  real ( kind = 8 ) fc
  real ( kind = 8 ) fd
  real ( kind = 8 ) fda
  real ( kind = 8 ) fdb
  logical first
  real ( kind = 8 ) ftol
  real ( kind = 8 ) fx
  integer ( kind = 4 ) i99999
  real ( kind = 8 ) m
  real ( kind = 8 ) mb
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  logical qrzero
  real ( kind = 8 ) reltol
  integer ( kind = 4 ) status
  real ( kind = 8 ) tol
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) xhi
  real ( kind = 8 ) xlo
  real ( kind = 8 ) :: xxhi = 0.0D+00
  real ( kind = 8 ) :: xxlo = 0.0D+00
  real ( kind = 8 ) zabstl
  real ( kind = 8 ) zreltl
  real ( kind = 8 ) zx
  real ( kind = 8 ) zxhi
  real ( kind = 8 ) zxlo

  save

  ftol(zx) = 0.5D+00 * max ( abstol, reltol * abs ( zx ) )

  if ( 0 < status ) then
    go to 280
  end if

  xlo = xxlo
  xhi = xxhi
  b = xlo
  x = xlo
!
!     GET-function-VALUE
!
  assign 10 to i99999
  go to 270

10 continue

  fb = fx
  xlo = xhi
  a = xlo
  x = xlo
!
!     GET-function-VALUE
!
  assign 20 to i99999
  go to 270
!
!  Check that F(ZXLO) < 0 < F(ZXHI)  or F(ZXLO) > 0 > F(ZXHI)
!
20 continue

  if ( fb < 0.0D+00 ) then
    if ( fx < 0.0D+00 ) then
      status = -1
      qleft = ( fx < fb )
      qhi = .false.
      return
    end if
  end if

  if ( 0.0D+00 < fb ) then
    if ( 0.0D+00 < fx ) then
      status = -1
      qleft = ( fb < fx )
      qhi = .true.
      return
    end if
  end if

  fa = fx
  first = .true.

70 continue

  c = a
  fc = fa
  ext = 0

80 continue

  if ( abs ( fc ) < abs ( fb ) ) then

    if ( c == a ) then
      d = a
      fd = fa
    end if

    a = b
    fa = fb
    xlo = c
    b = xlo
    fb = fc
    c = a
    fc = fa

  end if

  tol = ftol ( xlo )
  m = ( c + b ) * 0.5D+00
  mb = m - b

  if (.not. ( tol < abs ( mb ) ) ) then
    go to 240
  end if

  if ( 3 < ext ) then
    w = mb
    go to 190
  end if

  110 continue

  tol = sign ( tol, mb )
  p = ( b - a ) * fb
!
!  I had to insert a rudimentary check on the divisions here
!  to avoid ninny errors, JVB, 09 June 2004.
!
  if ( first ) then

    q = fa - fb
    first = .false.

  else

    if ( d == b ) then
      fdb = 1.0D+00
    else
      fdb = ( fd - fb ) / ( d - b )
    end if

    if ( d == a ) then
      fda = 1.0D+00
    else
      fda = ( fd - fa ) / ( d - a )
    end if

    p = fda * p
    q = fdb * fa - fda * fb

  end if

  130 continue

  if ( p < 0.0D+00 ) then
    p = -p
    q = -q
  end if

  140 continue

  if ( ext == 3 ) then
    p = p *  2.0D+00
  end if

  if (.not. ( ( p * 1.0D+00 ) == 0.0D+00 .or. p <= ( q * tol ) ) ) then
    go to 150
  end if

  w = tol
  go to 180

  150 continue

  if ( p < mb * q ) then
    w = p / q
  else
    w = mb
  end if

  180 continue
  190 continue

  d = a
  fd = fa
  a = b
  fa = fb
  b = b + w
  xlo = b
  x = xlo
!
!  GET-function-VALUE
!
  assign 200 to i99999
  go to 270

  200 continue

  fb = fx

  if ( 0.0D+00 <= fc * fb ) then

    go to 70

  else

    if ( w == mb ) then
      ext = 0
    else
      ext = ext + 1
    end if

    go to 80

  end if

  240 continue

  xhi = c
  qrzero = ( 0.0D+00 <= fc .and. fb <= 0.0D+00 ) .or. &
    ( fc < 0.0D+00 .and. fb >= 0.0D+00 )

  if ( qrzero ) then
    status = 0
  else
    status = -1
  end if

  return

entry dstzr ( zxlo, zxhi, zabstl, zreltl )

!*****************************************************************************80
!
!! DSTZR - SeT ZeRo finder - Reverse communication version
!
!  Discussion:
!
!    This routine sets quantities needed by ZROR.  The function of ZROR
!    and the quantities set is given here.
!
!    Given a function F, find XLO such that F(XLO) = 0.
!
!     Input condition. F is a real ( kind = 8 ) function of a single
!     real ( kind = 8 ) argument and XLO and XHI are such that
!          F(XLO)*F(XHI)  <=  0.0
!
!     If the input condition is met, QRZERO returns .TRUE.
!     and output values of XLO and XHI satisfy the following
!          F(XLO)*F(XHI)  <= 0.
!          ABS ( F(XLO) ) <= ABS ( F(XHI) )
!          ABS ( XLO - XHI ) <= TOL(X)
!     where
!          TOL(X) = MAX ( ABSTOL, RELTOL * ABS ( X ) )
!
!     If this algorithm does not find XLO and XHI satisfying
!     these conditions then QRZERO returns .FALSE.  This
!     implies that the input condition was not met.
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
!    Input, real ( kind = 8 ) XLO, XHI, the left and right endpoints of the
!    interval to be searched for a solution.
!
!    Input, real ( kind = 8 ) ABSTOL, RELTOL, two numbers that determine
!    the accuracy of the solution.
!
  xxlo = zxlo
  xxhi = zxhi
  abstol = zabstl
  reltol = zreltl
  return
!
!     TO GET-function-VALUE
!
  270 status = 1
  return

  280 continue
  go to i99999

end

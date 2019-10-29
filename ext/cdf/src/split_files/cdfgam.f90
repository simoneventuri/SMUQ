subroutine cdfgam ( which, p, q, x, shape, scale, status, bound )

!*****************************************************************************80
!
!! CDFGAM evaluates the CDF of the Gamma Distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the Gamma distribution
!    given the others.
!
!    The cumulative distribution function P is calculated directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The gamma density is proportional to T**(SHAPE - 1) * EXP(- SCALE * T)
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
!    Armido DiDinato, Alfred Morris,
!    Algorithm 654:
!    Computation of the incomplete gamma function ratios and their inverse,
!    ACM Transactions on Mathematical Software,
!    Volume 12, 1986, pages 377-393.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, SHAPE and SCALE;
!    2: Calculate X from P, Q, SHAPE and SCALE;
!    3: Calculate SHAPE from P, Q, X and SCALE;
!    4: Calculate SCALE from P, Q, X and SHAPE.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of the
!    Gamma density.  If this is an input value, it should lie in the
!    range: [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration of
!    the Gamma density.  If this is an input value, it should lie in the
!    range: [0, +infinity).  If it is an output value, it will lie in
!    the range: [0,1E300].
!
!    Input/output, real ( kind = 8 ) SHAPE, the shape parameter of the
!    Gamma density.  If this is an input value, it should lie in the range:
!    (0, +infinity).  If it is an output value, it will be searched for
!    in the range: [1.0D-300,1.0D+300].
!
!    Input/output, real ( kind = 8 ) SCALE, the scale parameter of the
!    Gamma density.  If this is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched for
!    in the range: (1.0D-300,1.0D+300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +10, if the Gamma or inverse Gamma routine cannot compute the answer.
!    This usually happens only for X and SHAPE very large (more than 1.0D+10.
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
  real ( kind = 8 ) fx
  integer ( kind = 4 ) ierr
  real ( kind = 8 ), parameter :: inf=1.0D+300
  real ( kind = 8 ) p
  real ( kind = 8 ) porq
  real ( kind = 8 ) q
  logical qhi
  logical qleft
  real ( kind = 8 ) scale
  real ( kind = 8 ) shape
  real ( kind = 8 ), parameter :: tol = 1.0D-08
  integer ( kind = 4 ) status,which
  real ( kind = 8 ) x
  real ( kind = 8 ) xscale
  real ( kind = 8 ) xx

  status = 0
  bound = 0.0D+00
!
!  Check the arguments.
!
  if ( which < 1 ) then
    bound = 1.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFGAM - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
    return
  end if

  if ( 4 < which ) then
    bound = 4.0D+00
    status = -1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CDFGAM - Fatal error!'
    write ( *, '(a)' ) '  The input parameter WHICH is out of range.'
    write ( *, '(a)' ) '  Legal values are between 1 and 4.'
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
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    else if ( 1.0D+00 < p ) then
      status = -2
      bound = 1.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter P is out of range.'
      return
    end if
  end if
!
!  Unless Q is to be computed, make sure it is legal.
!
  if ( which /= 1 ) then
    if ( q < 0.0D+00 ) then
      status = -3
      bound = 0.0D+00
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    else if ( 1.0D+00 < q ) then
      status = -3
      bound = 1.0
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter Q is out of range.'
      return
    end if
  end if
!
!  Unless X is to be computed, make sure it is legal.
!
  if ( which /= 2 ) then
    if ( x < 0.0D+00 ) then
      bound = 0.0D+00
      status = -4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter X is out of range.'
      return
    end if
  end if
!
!  Unless SHAPE is to be computed, make sure it is legal.
!
  if ( which /= 3 ) then
    if ( shape <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -5
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter SHAPE is out of range.'
      return
    end if
  end if
!
!  Unless SCALE is to be computed, make sure it is legal.
!
  if ( which /= 4 ) then
    if ( scale <= 0.0D+00 ) then
      bound = 0.0D+00
      status = -6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  Input parameter SCALE is out of range.'
      return
    end if
  end if
!
!  Check that P + Q = 1.
!
  if ( which /= 1 ) then
    if ( 3.0D+00 * epsilon ( p ) < abs ( ( p + q ) - 1.0D+0 ) ) then
      status = 3
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CDFGAM - Fatal error!'
      write ( *, '(a)' ) '  P + Q /= 1.'
      return
    end if
  end if
!
!  Select the minimum of P or Q.
!
  if ( which /= 1 ) then
    porq = min ( p, q )
  end if
!
!  Calculate P and Q.
!
  if ( which == 1 ) then

    status = 0
    xscale = x * scale
    call cumgam ( xscale, shape, p, q )

    if ( 1.5D+00 < porq ) then
      status = 10
    end if
!
!  Calculate X.
!
  else if ( which == 2 ) then

    call gamma_inc_inv ( shape, xx, -1.0D+00, p, q, ierr )

    if ( ierr < 0.0D+00 ) then
      status = 10
      return
    end if

    x = xx / scale
    status = 0
!
!  Calculate SHAPE.
!
  else if ( which == 3 ) then

    xscale = x * scale

    call dstinv ( 0.0D+00, inf, 0.5D+00, 0.5D+00, 5.0D+00, atol, tol )

    status = 0
    shape = 5.0D+00
    fx = 0.0D+00

    call dinvr ( status, shape, fx, qleft, qhi )

    do while ( status == 1 )

      call cumgam ( xscale, shape, cum, ccum )

      if ( p <= q ) then
        fx = cum - p
      else
        fx = ccum - q
      end if

      if ( p <= q .and. 1.5D+00 < cum ) then
        status = 10
        return
      else if ( q < p .and. 1.5D+00 < ccum ) then
        status = 10
        return
      end if

      call dinvr ( status, shape, fx, qleft, qhi )

    end do

    if ( status == -1 ) then
      if ( qleft ) then
        status = 1
        bound = 0.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFGAM - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be lower than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      else
        status = 2
        bound = inf
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CDFGAM - Warning!'
        write ( *, '(a)' ) '  The desired answer appears to be higher than'
        write ( *, '(a,g14.6)' ) '  the search bound of ', bound
      end if
    end if
!
!  Calculate SCALE.
!
  else if ( which == 4 ) then

    call gamma_inc_inv ( shape, xx, -1.0D+00, p, q, ierr )

    if ( ierr < 0.0D+00 ) then
      status = 10
    else
      scale = xx / x
      status = 0
    end if

  end if

  return
end

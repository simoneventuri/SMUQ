function psi ( xx )

!*****************************************************************************80
!
!! PSI evaluates the psi or digamma function, d/dx ln(gamma(x)).
!
!  Discussion:
!
!    The main computation involves evaluation of rational Chebyshev
!    approximations.  PSI was written at Argonne National Laboratory
!    for FUNPACK, and subsequently modified by A. H. Morris of NSWC.
!
!  Reference:
!
!    William Cody, Anthony Strecok, Henry Thacher,
!    Chebyshev Approximations for the Psi Function,
!    Mathematics of Computation,
!    Volume 27, 1973, pages 123-127.
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XX, the argument of the psi function.
!
!    Output, real ( kind = 8 ) PSI, the value of the psi function.  PSI
!    is assigned the value 0 when the psi function is undefined.
!
  implicit none

  real ( kind = 8 ) aug
  real ( kind = 8 ) den
  real ( kind = 8 ), parameter :: dx0 = &
    1.461632144968362341262659542325721325D+00
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ipmpar
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nq
  real ( kind = 8 ), parameter, dimension ( 7 ) :: p1 = (/ &
   0.895385022981970D-02, &
   0.477762828042627D+01, &
   0.142441585084029D+03, &
   0.118645200713425D+04, &
   0.363351846806499D+04, &
   0.413810161269013D+04, &
   0.130560269827897D+04/)
  real ( kind = 8 ), dimension ( 4 ) :: p2 = (/ &
    -0.212940445131011D+01, &
    -0.701677227766759D+01, &
    -0.448616543918019D+01, &
    -0.648157123766197D+00 /)
  real ( kind = 8 ), parameter :: piov4 = 0.785398163397448D+00
  real ( kind = 8 ) psi
!
!  Coefficients for rational approximation of
!  PSI(X) / (X - X0),  0.5D+00 <= X <= 3.0D+00
!
  real ( kind = 8 ), dimension ( 6 ) :: q1 = (/ &
    0.448452573429826D+02, &
    0.520752771467162D+03, &
    0.221000799247830D+04, &
    0.364127349079381D+04, &
    0.190831076596300D+04, &
    0.691091682714533D-05 /)
  real ( kind = 8 ), dimension ( 4 ) :: q2 = (/ &
    0.322703493791143D+02, &
    0.892920700481861D+02, &
    0.546117738103215D+02, &
    0.777788548522962D+01 /)
  real ( kind = 8 ) sgn
  real ( kind = 8 ) upper
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) xmax1
  real ( kind = 8 ) xmx0
  real ( kind = 8 ) xsmall
  real ( kind = 8 ) xx
  real ( kind = 8 ) z
!
!  XMAX1 is the largest positive floating point constant with entirely
!  integer representation.  It is also used as negative of lower bound
!  on acceptable negative arguments and as the positive argument beyond which
!  psi may be represented as LOG(X).
!
  xmax1 = real ( ipmpar(3), kind = 8 )
  xmax1 = min ( xmax1, 1.0D+00 / epsilon ( xmax1 ) )
!
!  XSMALL is the absolute argument below which PI*COTAN(PI*X)
!  may be represented by 1/X.
!
  xsmall = 1.0D-09

  x = xx
  aug = 0.0D+00

  if ( x == 0.0D+00 ) then
    psi = 0.0D+00
    return
  end if
!
!  X < 0.5,  Use reflection formula PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
!
  if ( x < 0.5D+00 ) then
!
!  0 < ABS ( X ) <= XSMALL.  Use 1/X as a substitute for PI*COTAN(PI*X)
!
    if ( abs ( x ) <= xsmall ) then
      aug = -1.0D+00 / x
      go to 40
    end if
!
!  Reduction of argument for cotangent.
!
    w = -x
    sgn = piov4

    if ( w <= 0.0D+00 ) then
      w = -w
      sgn = -sgn
    end if
!
!  Make an error exit if X <= -XMAX1
!
    if ( xmax1 <= w ) then
      psi = 0.0D+00
      return
    end if

    nq = int ( w )
    w = w - real ( nq, kind = 8 )
    nq = int ( w * 4.0D+00 )
    w = 4.0D+00 * ( w - real ( nq, kind = 8 ) * 0.25D+00 )
!
!  W is now related to the fractional part of 4.0D+00 * X.
!  Adjust argument to correspond to values in first
!  quadrant and determine sign.
!
    n = nq / 2
    if ( n + n /= nq ) then
      w = 1.0D+00 - w
    end if

    z = piov4 * w
    m = n / 2

    if ( m + m /= n ) then
      sgn = -sgn
    end if
!
!  Determine final value for -PI * COTAN(PI*X).
!
    n = ( nq + 1 ) / 2
    m = n / 2
    m = m + m

    if ( m == n ) then

      if ( z == 0.0D+00 ) then
        psi = 0.0D+00
        return
      end if

      aug = 4.0D+00 * sgn * ( cos(z) / sin(z) )

    else

      aug = 4.0D+00 * sgn * ( sin(z) / cos(z) )

    end if

   40   continue

    x = 1.0D+00 - x

  end if
!
!  0.5 <= X <= 3
!
  if ( x <= 3.0D+00 ) then

    den = x
    upper = p1(1) * x

    do i = 1, 5
      den = ( den + q1(i) ) * x
      upper = ( upper + p1(i+1) ) * x
    end do

    den = ( upper + p1(7) ) / ( den + q1(6) )
    xmx0 = real ( x, kind = 8 ) - dx0
    psi = den * xmx0 + aug
!
!  3 < X < XMAX1
!
  else if ( x < xmax1 ) then

    w = 1.0D+00 / x**2
    den = w
    upper = p2(1) * w

    do i = 1, 3
      den = ( den + q2(i) ) * w
      upper = ( upper + p2(i+1) ) * w
    end do

    aug = upper / ( den + q2(4) ) - 0.5D+00 / x + aug
    psi = aug + log ( x )
!
!  XMAX1 <= X
!
  else

    psi = aug + log ( x )

  end if

  return
end

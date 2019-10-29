function beta_inc ( a, b, x )

!*****************************************************************************80
!
!! BETA_INC returns the value of the incomplete Beta function.
!
!  Discussion:
!
!    This calculation requires an iteration.  In some cases, the iteration
!    may not converge rapidly, or may become inaccurate.
!
!    The formula is:
!
!      BETA_INC(A,B,X)
!
!        =   Integral ( 0 <= T <= X ) T^(A-1) (1-T)^(B-1) dT
!          / Integral ( 0 <= T <= 1 ) T^(A-1) (1-T)^(B-1) dT
!
!        =   Integral ( 0 <= T <= X ) T^(A-1) (1-T)^(B-1) dT
!          / BETA(A,B)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2004
!
!  Author:
!
!    Original FORTRAN77 version by KL Majumder, GP Bhattacharjee.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    KL Majumder, GP Bhattacharjee,
!    Algorithm AS63,
!    Applied Statistics,
!    1973, volume 22, number 3.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    0.0 < A,
!    0.0 < B.
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!    Normally, 0.0D+00 <= X <= 1.0.
!
!    Output, real ( kind = 8 ) BETA_INC, the value of the function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) beta_inc
  real ( kind = 8 ) cx
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  integer ( kind = 4 ), parameter :: it_max = 1000
  logical indx
  integer ( kind = 4 ) ns
  real ( kind = 8 ) pp
  real ( kind = 8 ) psq
  real ( kind = 8 ) qq
  real ( kind = 8 ) r8_beta
  real ( kind = 8 ) rx
  real ( kind = 8 ) temp
  real ( kind = 8 ) term
  real ( kind = 8 ), parameter :: tol = 1.0D-07
  real ( kind = 8 ) x
  real ( kind = 8 ) xx

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    stop 1
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    stop 1
  end if

  if ( x <= 0.0D+00 ) then
    beta_inc = 0.0D+00
    return
  else if ( 1.0D+00 <= x ) then
    beta_inc = 1.0D+00
    return
  end if
!
!  Change tail if necessary and determine S.
!
  psq = a + b

  if ( a < ( a + b ) * x ) then
    xx = 1.0D+00 - x
    cx = x
    pp = b
    qq = a
    indx = .true.
  else
    xx = x
    cx = 1.0D+00 - x
    pp = a
    qq = b
    indx = .false.
  end if

  term = 1.0D+00
  i = 1
  beta_inc = 1.0D+00

  ns = int ( qq + cx * ( a + b ) )
!
!  Use Soper's reduction formulas.
!
  rx = xx / cx

  temp = qq - real ( i, kind = 8 )
  if ( ns == 0 ) then
    rx = xx
  end if

  it = 0

  do

    it = it + 1

    if ( it_max < it ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_INC - Fatal error!'
      write ( *, '(a)' ) '  Maximum number of iterations exceeded!'
      write ( *, '(a,i8)' ) '  IT_MAX = ', it_max
      stop 1
    end if

    term = term * temp * rx / ( pp + real ( i, kind = 8 ) )
    beta_inc = beta_inc + term
    temp = abs ( term )

    if ( temp <= tol .and. temp <= tol * beta_inc ) then
      exit
    end if

    i = i + 1
    ns = ns - 1

    if ( 0 <= ns ) then
      temp = qq - real ( i, kind = 8 )
      if ( ns == 0 ) then
        rx = xx
      end if
    else
      temp = psq
      psq = psq + 1.0D+00
    end if

  end do
!
!  Finish calculation.
!
  beta_inc = beta_inc * exp ( pp * log ( xx ) &
    + ( qq - 1.0D+00 ) * log ( cx ) ) / ( r8_beta ( a, b ) * pp )

  if ( indx ) then
    beta_inc = 1.0D+00 - beta_inc
  end if

  return
end

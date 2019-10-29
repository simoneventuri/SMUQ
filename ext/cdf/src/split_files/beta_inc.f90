subroutine beta_inc ( a, b, x, y, w, w1, ierr )

!*****************************************************************************80
!
!! BETA_INC evaluates the incomplete beta function IX(A,B).
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Output, real ( kind = 8 ) W, W1, the values of IX(A,B) and
!    1-IX(A,B).
!
!    Output, integer ( kind = 4 ) IERR, the error flag.
!    0, no error was detected.
!    1, A or B is negative;
!    2, A = B = 0;
!    3, X < 0 or 1 < X;
!    4, Y < 0 or 1 < Y;
!    5, X + Y /= 1;
!    6, X = A = 0;
!    7, Y = B = 0.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) apser
  real ( kind = 8 ) b
  real ( kind = 8 ) b0
  real ( kind = 8 ) beta_asym
  real ( kind = 8 ) beta_frac
  real ( kind = 8 ) beta_pser
  real ( kind = 8 ) beta_up
  real ( kind = 8 ) eps
  real ( kind = 8 ) fpser
  integer ( kind = 4 ) ierr
  integer ( kind = 4 ) ierr1
  integer ( kind = 4 ) ind
  real ( kind = 8 ) lambda
  integer ( kind = 4 ) n
  real ( kind = 8 ) t
  real ( kind = 8 ) w
  real ( kind = 8 ) w1
  real ( kind = 8 ) x
  real ( kind = 8 ) x0
  real ( kind = 8 ) y
  real ( kind = 8 ) y0
  real ( kind = 8 ) z

  eps = epsilon ( eps )
  w = 0.0D+00
  w1 = 0.0D+00

  if ( a < 0.0D+00 .or. b < 0.0D+00 ) then
    ierr = 1
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  if ( a == 0.0D+00 .and. b == 0.0D+00 ) then
    ierr = 2
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  if ( x < 0.0D+00 .or. 1.0D+00 < x ) then
    ierr = 3
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  if ( y < 0.0D+00 .or. 1.0D+00 < y ) then
    ierr = 4
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  z = ( ( x + y ) - 0.5D+00 ) - 0.5D+00

  if ( 3.0D+00 * eps < abs ( z ) ) then
    ierr = 5
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BETA_INC - Fatal error!'
    write ( *, '(a,i8)' ) '  IERR = ', ierr
    return
  end if

  ierr = 0

  if ( x == 0.0D+00 ) then
    w = 0.0D+00
    w1 = 1.0D+00
    if ( a == 0.0D+00 ) then
      ierr = 6
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_INC - Fatal error!'
      write ( *, '(a,i8)' ) '  IERR = ', ierr
    end if
    return
  end if

  if ( y == 0.0D+00 ) then
    if ( b == 0.0D+00 ) then
      ierr = 7
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BETA_INC - Fatal error!'
      write ( *, '(a,i8)' ) '  IERR = ', ierr
      return
    end if
    w = 1.0D+00
    w1 = 0.0D+00
    return
  end if

  if ( a == 0.0D+00 ) then
    w = 1.0D+00
    w1 = 0.0D+00
    return
  end if

  if ( b == 0.0D+00 ) then
    w = 0.0D+00
    w1 = 1.0D+00
    return
  end if

  eps = max ( eps, 1.0D-15 )

  if ( max ( a, b ) < 0.001D+00 * eps ) then
    go to 260
  end if

  ind = 0
  a0 = a
  b0 = b
  x0 = x
  y0 = y

  if ( 1.0D+00 < min ( a0, b0 ) ) then
    go to 40
  end if
!
!  Procedure for A0 <= 1 or B0 <= 1
!
  if ( 0.5D+00 < x ) then
    ind = 1
    a0 = b
    b0 = a
    x0 = y
    y0 = x
  end if

  if ( b0 < min ( eps, eps * a0 ) ) then
    go to 90
  end if

  if ( a0 < min ( eps, eps * b0 ) .and. b0 * x0 <= 1.0D+00 ) then
    go to 100
  end if

  if ( 1.0D+00 < max ( a0, b0 ) ) then
    go to 20
  end if

  if ( min ( 0.2D+00, b0 ) <= a0 ) then
    go to 110
  end if

  if ( x0**a0 <= 0.9D+00 ) then
    go to 110
  end if

  if ( 0.3D+00 <= x0 ) then
    go to 120
  end if

  n = 20
  go to 140

20 continue

  if ( b0 <= 1.0D+00 ) then
    go to 110
  end if

  if ( 0.3D+00 <= x0 ) then
    go to 120
  end if

  if ( 0.1D+00 <= x0 ) then
    go to 30
  end if

  if ( ( x0 * b0 )**a0 <= 0.7D+00 ) then
    go to 110
  end if

30 continue

  if ( 15.0D+00 < b0 ) then
    go to 150
  end if

  n = 20
  go to 140
!
!  PROCEDURE for 1 < A0 and 1 < B0.
!
40 continue

  if ( a <= b ) then
    lambda = a - ( a + b ) * x
  else
    lambda = ( a + b ) * y - b
  end if

  if ( lambda < 0.0D+00 ) then
    ind = 1
    a0 = b
    b0 = a
    x0 = y
    y0 = x
    lambda = abs ( lambda )
  end if

70 continue

  if ( b0 < 40.0D+00 .and. b0 * x0 <= 0.7D+00 ) then
    go to 110
  end if

  if ( b0 < 40.0D+00 ) then
    go to 160
  end if

  if ( b0 < a0 ) then
    go to 80
  end if

  if ( a0 <= 100.0D+00 ) then
    go to 130
  end if

  if ( 0.03D+00 * a0 < lambda ) then
    go to 130
  end if

  go to 200

80 continue

  if ( b0 <= 100.0D+00 ) then
    go to 130
  end if

  if ( 0.03D+00 * b0 < lambda ) then
    go to 130
  end if

  go to 200
!
!  Evaluation of the appropriate algorithm.
!
90 continue

  w = fpser ( a0, b0, x0, eps )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250

100 continue

  w1 = apser ( a0, b0, x0, eps )
  w = 0.5D+00 + ( 0.5D+00 - w1 )
  go to 250

110 continue

  w = beta_pser ( a0, b0, x0, eps )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250

120 continue

  w1 = beta_pser ( b0, a0, y0, eps )
  w = 0.5D+00 + ( 0.5D+00 - w1 )
  go to 250

130 continue

  w = beta_frac ( a0, b0, x0, y0, lambda, 15.0D+00 * eps )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250

140 continue

  w1 = beta_up ( b0, a0, y0, x0, n, eps )
  b0 = b0 + n

150 continue

  call beta_grat ( b0, a0, y0, x0, w1, 15.0D+00 * eps, ierr1 )
  w = 0.5D+00 + ( 0.5D+00 - w1 )
  go to 250

160 continue

  n = b0
  b0 = b0 - n

  if ( b0 == 0.0D+00 ) then
    n = n - 1
    b0 = 1.0D+00
  end if

170 continue

  w = beta_up ( b0, a0, y0, x0, n, eps )

  if ( x0 <= 0.7D+00 ) then
    w = w + beta_pser ( a0, b0, x0, eps )
    w1 = 0.5D+00 + ( 0.5D+00 - w )
    go to 250
  end if

  if ( a0 <= 15.0D+00 ) then
    n = 20
    w = w + beta_up ( a0, b0, x0, y0, n, eps )
    a0 = a0 + n
  end if

190 continue

  call beta_grat ( a0, b0, x0, y0, w, 15.0D+00 * eps, ierr1 )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250

200 continue

  w = beta_asym ( a0, b0, lambda, 100.0D+00 * eps )
  w1 = 0.5D+00 + ( 0.5D+00 - w )
  go to 250
!
!  Termination of the procedure.
!
250 continue

  if ( ind /= 0 ) then
    t = w
    w = w1
    w1 = t
  end if

  return
!
!  Procedure for A and B < 0.001 * EPS
!
260 continue

  w = b / ( a + b )
  w1 = a / ( a + b )

  return
end

function gamma ( a )

!*****************************************************************************80
!
!! GAMMA evaluates the gamma function.
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
!    Input, real ( kind = 8 ) A, the argument of the Gamma function.
!
!    Output, real ( kind = 8 ) GAMMA, the value of the Gamma function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) bot
  real ( kind = 8 ), parameter :: d = 0.41893853320467274178D+00
  real ( kind = 8 ) exparg
  real ( kind = 8 ) g
  real ( kind = 8 ) gamma
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) lnx
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ), dimension ( 7 ) :: p = (/ &
    0.539637273585445D-03, 0.261939260042690D-02, &
    0.204493667594920D-01, 0.730981088720487D-01, &
    0.279648642639792D+00, 0.553413866010467D+00, &
    1.0D+00 /)
  real ( kind = 8 ), parameter :: pi = 3.1415926535898D+00
  real ( kind = 8 ), dimension ( 7 ) :: q = (/ &
    -0.832979206704073D-03,  0.470059485860584D-02, &
     0.225211131035340D-01, -0.170458969313360D+00, &
    -0.567902761974940D-01,  0.113062953091122D+01, &
     1.0D+00 /)
  real ( kind = 8 ), parameter :: r1 =  0.820756370353826D-03
  real ( kind = 8 ), parameter :: r2 = -0.595156336428591D-03
  real ( kind = 8 ), parameter :: r3 =  0.793650663183693D-03
  real ( kind = 8 ), parameter :: r4 = -0.277777777770481D-02
  real ( kind = 8 ), parameter :: r5 =  0.833333333333333D-01
  real ( kind = 8 ) s
  real ( kind = 8 ) t
  real ( kind = 8 ) top
  real ( kind = 8 ) w
  real ( kind = 8 ) x
  real ( kind = 8 ) z

  gamma = 0.0D+00
  x = a

  if ( abs ( a ) < 15.0D+00 ) then
!
!  Evaluation of GAMMA(A) for |A| < 15
!
    t = 1.0D+00
    m = int ( a ) - 1
!
!  Let T be the product of A-J when 2 <= A.
!
    if ( 0 <= m ) then

      do j = 1, m
        x = x - 1.0D+00
        t = x * t
      end do

      x = x - 1.0D+00
!
!  Let T be the product of A+J WHEN A < 1
!
    else

      t = a

      if ( a <= 0.0D+00 ) then

        m = - m - 1

        do j = 1, m
          x = x + 1.0D+00
          t = x * t
        end do

        x = ( x + 0.5D+00 ) + 0.5D+00
        t = x * t
        if ( t == 0.0D+00 ) then
          return
        end if

      end if
!
!  Check if 1/T can overflow.
!
      if ( abs ( t ) < 1.0D-30 ) then
        if ( 1.0001D+00 < abs ( t ) * huge ( t ) ) then
          gamma = 1.0D+00 / t
        end if
        return
      end if

    end if
!
!  Compute Gamma(1 + X) for 0 <= X < 1.
!
    top = p(1)
    bot = q(1)
    do i = 2, 7
      top = top * x + p(i)
      bot = bot * x + q(i)
    end do

    gamma = top / bot
!
!  Termination.
!
    if ( 1.0D+00 <= a ) then
      gamma = gamma * t
    else
      gamma = gamma / t
    end if
!
!  Evaluation of Gamma(A) FOR 15 <= ABS ( A ).
!
  else

    if ( 1000.0D+00 <= abs ( a ) ) then
      return
    end if

    if ( a <= 0.0D+00 ) then

      x = -a
      n = x
      t = x - n

      if ( 0.9D+00 < t ) then
        t = 1.0D+00 - t
      end if

      s = sin ( pi * t ) / pi

      if ( mod ( n, 2 ) == 0 ) then
        s = -s
      end if

      if ( s == 0.0D+00 ) then
        return
      end if

    end if
!
!  Compute the modified asymptotic sum.
!
    t = 1.0D+00 / ( x * x )

    g = (((( r1 * t + r2 ) * t + r3 ) * t + r4 ) * t + r5 ) / x

    lnx = log ( x )
!
!  Final assembly.
!
    z = x
    g = ( d + g ) + ( z - 0.5D+00 ) &
      * ( lnx - 1.0D+00 )
    w = g
    t = g - real ( w, kind = 8 )

    if ( 0.99999D+00 * exparg ( 0 ) < w ) then
      return
    end if

    gamma = exp ( w )* ( 1.0D+00 + t )

    if ( a < 0.0D+00 ) then
      gamma = ( 1.0D+00 / ( gamma * s ) ) / x
    end if

  end if

  return
end

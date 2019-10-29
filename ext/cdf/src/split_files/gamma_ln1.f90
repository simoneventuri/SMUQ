function gamma_ln1 ( a )

!*****************************************************************************80
!
!! GAMMA_LN1 evaluates ln ( Gamma ( 1 + A ) ), for -0.2 <= A <= 1.25.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
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
!    Input, real ( kind = 8 ) A, defines the argument of the function.
!
!    Output, real ( kind = 8 ) GAMMA_LN1, the value of ln ( Gamma ( 1 + A ) ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) bot
  real ( kind = 8 ) gamma_ln1
  real ( kind = 8 ), parameter :: p0 =  0.577215664901533D+00
  real ( kind = 8 ), parameter :: p1 =  0.844203922187225D+00
  real ( kind = 8 ), parameter :: p2 = -0.168860593646662D+00
  real ( kind = 8 ), parameter :: p3 = -0.780427615533591D+00
  real ( kind = 8 ), parameter :: p4 = -0.402055799310489D+00
  real ( kind = 8 ), parameter :: p5 = -0.673562214325671D-01
  real ( kind = 8 ), parameter :: p6 = -0.271935708322958D-02
  real ( kind = 8 ), parameter :: q1 =  0.288743195473681D+01
  real ( kind = 8 ), parameter :: q2 =  0.312755088914843D+01
  real ( kind = 8 ), parameter :: q3 =  0.156875193295039D+01
  real ( kind = 8 ), parameter :: q4 =  0.361951990101499D+00
  real ( kind = 8 ), parameter :: q5 =  0.325038868253937D-01
  real ( kind = 8 ), parameter :: q6 =  0.667465618796164D-03
  real ( kind = 8 ), parameter :: r0 = 0.422784335098467D+00
  real ( kind = 8 ), parameter :: r1 = 0.848044614534529D+00
  real ( kind = 8 ), parameter :: r2 = 0.565221050691933D+00
  real ( kind = 8 ), parameter :: r3 = 0.156513060486551D+00
  real ( kind = 8 ), parameter :: r4 = 0.170502484022650D-01
  real ( kind = 8 ), parameter :: r5 = 0.497958207639485D-03
  real ( kind = 8 ), parameter :: s1 = 0.124313399877507D+01
  real ( kind = 8 ), parameter :: s2 = 0.548042109832463D+00
  real ( kind = 8 ), parameter :: s3 = 0.101552187439830D+00
  real ( kind = 8 ), parameter :: s4 = 0.713309612391000D-02
  real ( kind = 8 ), parameter :: s5 = 0.116165475989616D-03
  real ( kind = 8 ) top
  real ( kind = 8 ) x

  if ( a < 0.6D+00 ) then

    top = (((((  &
            p6   &
      * a + p5 ) &
      * a + p4 ) &
      * a + p3 ) &
      * a + p2 ) &
      * a + p1 ) &
      * a + p0

    bot = (((((  &
            q6   &
      * a + q5 ) &
      * a + q4 ) &
      * a + q3 ) &
      * a + q2 ) &
      * a + q1 ) &
      * a + 1.0D+00

    gamma_ln1 = -a * ( top / bot )

  else

    x = ( a - 0.5D+00 ) - 0.5D+00

    top = ((((( r5 * x + r4 ) * x + r3 ) * x + r2 ) * x + r1 ) * x + r0 )

    bot = ((((( s5 * x + s4 ) * x + s3 ) * x + s2 ) * x + s1 ) * x + 1.0D+00 )

    gamma_ln1 = x * ( top / bot )

  end if

  return
end

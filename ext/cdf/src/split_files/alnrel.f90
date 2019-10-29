function alnrel ( a )

!*****************************************************************************80
!
!! ALNREL evaluates the function ln ( 1 + A ).
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
!    Input, real ( kind = 8 ) A, the argument.
!
!    Output, real ( kind = 8 ) ALNREL, the value of ln ( 1 + A ).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) alnrel
  real ( kind = 8 ), parameter :: p1 = -0.129418923021993D+01
  real ( kind = 8 ), parameter :: p2 =  0.405303492862024D+00
  real ( kind = 8 ), parameter :: p3 = -0.178874546012214D-01
  real ( kind = 8 ), parameter :: q1 = -0.162752256355323D+01
  real ( kind = 8 ), parameter :: q2 =  0.747811014037616D+00
  real ( kind = 8 ), parameter :: q3 = -0.845104217945565D-01
  real ( kind = 8 ) t
  real ( kind = 8 ) t2
  real ( kind = 8 ) w
  real ( kind = 8 ) x

  if ( abs ( a ) <= 0.375D+00 ) then

    t = a / ( a +  2.0D+00  )
    t2 = t * t

    w = ((( p3 * t2 + p2 ) * t2 + p1 ) * t2 + 1.0D+00 ) &
      / ((( q3 * t2 + q2 ) * t2 + q1 ) * t2 + 1.0D+00 )

    alnrel =  2.0D+00  * t * w

  else

    x = 1.0D+00 + real ( a, kind = 8 )
    alnrel = log ( x )

  end if

  return
end

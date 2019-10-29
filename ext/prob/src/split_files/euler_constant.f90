function euler_constant ( )

!*****************************************************************************80
!
!! EULER_CONSTANT returns the value of the Euler-Mascheroni constant.
!
!  Discussion:
!
!    The Euler-Mascheroni constant is often denoted by a lower-case
!    Gamma.  Gamma is defined as
!
!      Gamma = limit ( M -> Infinity )
!        ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) EULER_CONSTANT, the value of the
!    Euler-Mascheroni constant.
!
  implicit none

  real ( kind = 8 ) euler_constant

  euler_constant = 0.577215664901532860606512090082402431042D+00

  return
end

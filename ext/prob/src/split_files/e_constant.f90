function e_constant ( )

!*****************************************************************************80
!
!! E_CONSTANT returns the value of E.
!
!  Discussion:
!
!   "E" was named in honor of Euler.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) E_CONSTANT, the base of the natural
!    logarithm system.
!
  implicit none

  real ( kind = 8 ) e_constant

  e_constant = 2.71828182845904523536028747135266249775724709369995D+00

  return
end

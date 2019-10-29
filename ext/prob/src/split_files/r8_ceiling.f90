function r8_ceiling ( r )

!*****************************************************************************80
!
!! R8_CEILING rounds an R8 "up" to the nearest integer.
!
!  Example:
!
!    R     Value
!
!   -1.1  -1
!   -1.0  -1
!   -0.9   0
!    0.0   0
!    5.0   5
!    5.1   6
!    5.9   6
!    6.0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the real value to be rounded up.
!
!    Output, integer ( kind = 4 ) R8_CEILING, the rounded value.
!
  implicit none

  integer ( kind = 4 ) r8_ceiling
  real ( kind = 8 ) r
  integer ( kind = 4 ) value

  value = int ( r )
  if ( real ( value, kind = 8 ) < r ) then
    value = value + 1
  end if

  r8_ceiling = value

  return
end

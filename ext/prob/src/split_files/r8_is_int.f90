function r8_is_int ( r )

!*****************************************************************************80
!
!! R8_IS_INT determines if an R8 represents an integer value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the number to be checked.
!
!    Output, logical R8_IS_INT, is TRUE if R is an integer value.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_huge
  real ( kind = 8 ) r
  logical r8_is_int

  if ( real ( i4_huge ( ), kind = 8 ) < r ) then
    r8_is_int = .false.
  else if ( r < - real ( i4_huge ( ) , kind = 8 ) ) then
    r8_is_int = .false.
  else if ( r == real ( int ( r ), kind = 8 ) ) then
    r8_is_int = .true.
  else
    r8_is_int = .false.
  end if

  return
end

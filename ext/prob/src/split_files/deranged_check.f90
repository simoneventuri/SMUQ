function deranged_check ( a )

!*****************************************************************************80
!
!! DERANGED_CHECK checks the parameter of the Deranged PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the total number of items.
!    1 <= A.
!
!    Output, logical DERANGED_CHECK, is true if the parameters are legal.
!
  implicit none

  integer ( kind = 4 ) a
  logical deranged_check

  if ( a < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DERANGED_CHECK - Warning!'
    write ( *, '(a)' ) '  A < 1.'
    deranged_check = .false.
    return
  end if

  deranged_check = .true.

  return
end

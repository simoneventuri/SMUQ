function f_check ( m, n )

!*****************************************************************************80
!
!! F_CHECK checks the parameters of the F PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Output, logical F_CHECK, is TRUE if the parameters are legal.
!
  implicit none

  logical f_check
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  if ( m < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F_CHECK - Warning!'
    write ( *, '(a)' ) '  M < 1.'
    f_check = .false.
    return
  end if

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F_CHECK - Warning!'
    write ( *, '(a)' ) '  N < 1.'
    f_check = .false.
    return
  end if

  f_check = .true.

  return
end

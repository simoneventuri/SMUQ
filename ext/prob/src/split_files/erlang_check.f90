function erlang_check ( a, b, c )

!*****************************************************************************80
!
!! ERLANG_CHECK checks the parameters of the Erlang PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, logical ERLANG_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  logical erlang_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ERLANG_CHECK - Warning!'
    write ( *, '(a)' ) '  B <= 0.0'
    erlang_check = .false.
    return
  end if

  if ( c <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ERLANG_CHECK - Warning!'
    write ( *, '(a)' ) '  C <= 0.'
    erlang_check = .false.
    return
  end if

  erlang_check = .true.

  return
end

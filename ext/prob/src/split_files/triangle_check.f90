function triangle_check ( a, b, c )

!*****************************************************************************80
!
!! TRIANGLE_CHECK checks the parameters of the Triangle CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, logical TRIANGLE_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical triangle_check

  if ( b < a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIANGLE_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B < A.'
    triangle_check = .false.
    return
  end if

  if ( c < b ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIANGLE_CHECK - Fatal error!'
    write ( *, '(a)' ) '  C < B.'
    triangle_check = .false.
    return
  end if

  if ( a == c ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIANGLE_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A == C.'
    triangle_check = .false.
    return
  end if

  triangle_check = .true.

  return
end

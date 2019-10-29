function folded_normal_check ( a, b )

!*****************************************************************************80
!
!! FOLDED_NORMAL_CHECK checks the parameters of the Folded Normal CDF.
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
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, logical FOLDED_NORMAL_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical folded_normal_check

  if ( a < 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FOLDED_NORMAL_CHECK - Warning!'
    write ( *, '(a)' ) '  A < 0.'
    folded_normal_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FOLDED_NORMAL_CHECK - Warning!'
    write ( *, '(a)' ) '  B <= 0.'
    folded_normal_check = .false.
    return
  end if

  folded_normal_check = .true.

  return
end

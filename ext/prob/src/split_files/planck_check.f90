function planck_check ( a, b )

!*****************************************************************************80
!
!! PLANCK_CHECK checks the parameters of the Planck PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, logical PLANCK_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical planck_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PLANCK_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    planck_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PLANCK_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    planck_check = .false.
    return
  end if

  planck_check = .true.

  return
end

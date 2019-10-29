function power_check ( a, b )

!*****************************************************************************80
!
!! POWER_CHECK checks the parameter of the Power PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, logical POWER_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical power_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POWER_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    power_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POWER_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    power_check = .false.
    return
  end if

  power_check = .true.

  return
end

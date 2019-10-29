function half_normal_check ( a, b )

!*****************************************************************************80
!
!! HALF_NORMAL_CHECK checks the parameters of the Half Normal PDF.
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
!    0.0 < B.
!
!    Output, logical HALF_NORMAL_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical half_normal_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HALF_NORMAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    half_normal_check = .false.
    return
  end if

  half_normal_check = .true.

  return
end

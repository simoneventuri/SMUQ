function normal_check ( a, b )

!*****************************************************************************80
!
!! NORMAL_CHECK checks the parameters of the Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 1999
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
!    Output, logical NORMAL_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical normal_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'NORMAL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    normal_check = .false.
    return
  end if

  normal_check = .true.

  return
end

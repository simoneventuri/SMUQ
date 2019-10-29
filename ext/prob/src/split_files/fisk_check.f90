function fisk_check ( a, b, c )

!*****************************************************************************80
!
!! FISK_CHECK checks the parameters of the Fisk PDF.
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
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, logical FISK_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical fisk_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FISK_CHECK - Warning!'
    write ( *, '(a)' ) '  B <= 0.'
    fisk_check = .false.
    return
  end if

  if ( c <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FISK_CHECK - Warning!'
    write ( *, '(a)' ) '  C <= 0.'
    fisk_check = .false.
    return
  end if

  fisk_check = .true.

  return
end

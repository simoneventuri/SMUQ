function burr_check ( a, b, c, d )

!*****************************************************************************80
!
!! BURR_CHECK checks the parameters of the Burr CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, logical BURR_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical burr_check
  real ( kind = 8 ) c
  real ( kind = 8 ) d

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BURR_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    burr_check = .false.
    return
  end if

  if ( c <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BURR_CHECK - Fatal error!'
    write ( *, '(a)' ) '  C <= 0.'
    burr_check = .false.
    return
  end if

  burr_check = .true.

  return
end

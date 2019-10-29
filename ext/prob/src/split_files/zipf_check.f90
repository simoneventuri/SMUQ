function zipf_check ( a )

!*****************************************************************************80
!
!! ZIPF_CHECK checks the parameter of the Zipf PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, logical ZIPF_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical zipf_check

  if ( a <= 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ZIPF_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 1.'
    zipf_check = .false.
    return
  end if

  zipf_check = .true.

  return
end

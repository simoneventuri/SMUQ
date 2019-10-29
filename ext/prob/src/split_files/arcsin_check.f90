function arcsin_check ( a )

!*****************************************************************************80
!
!! ARCSIN_CHECK checks the parameter of the Arcsin CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, logical ARCSIN_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical arcsin_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ARCSIN_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    arcsin_check = .false.
    return
  end if

  arcsin_check = .true.

  return
end

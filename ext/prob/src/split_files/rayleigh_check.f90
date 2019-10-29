function rayleigh_check ( a )

!*****************************************************************************80
!
!! RAYLEIGH_CHECK checks the parameter of the Rayleigh PDF.
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
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, logical RAYLEIGH_CHECK, is true if the parameter is legal.
!
  implicit none

  real ( kind = 8 ) a
  logical rayleigh_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'RAYLEIGH_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    rayleigh_check = .false.
    return
  end if

  rayleigh_check = .true.

  return
end

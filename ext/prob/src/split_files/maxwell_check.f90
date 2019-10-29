function maxwell_check ( a )

!*****************************************************************************80
!
!! MAXWELL_CHECK checks the parameters of the Maxwell CDF.
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
!    0 < A.
!
!    Output, logical MAXWELL_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical maxwell_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MAXWELL_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.0.'
    maxwell_check = .false.
    return
  end if

  maxwell_check = .true.

  return
end

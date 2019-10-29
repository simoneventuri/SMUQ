function chi_square_check ( a )

!*****************************************************************************80
!
!! CHI_SQUARE_CHECK checks the parameter of the central Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the distribution.
!    1 <= A.
!
!    Output, logical CHI_SQUARE_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical chi_square_check

  if ( a < 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_SQUARE_CHECK - Warning!'
    write ( *, '(a)' ) '  A < 1.0.'
    chi_square_check = .false.
    return
  end if

  chi_square_check = .true.

  return
end

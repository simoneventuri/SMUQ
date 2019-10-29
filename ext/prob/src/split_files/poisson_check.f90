function poisson_check ( a )

!*****************************************************************************80
!
!! POISSON_CHECK checks the parameter of the Poisson PDF.
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
!    Output, logical POISSON_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical poisson_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'POISSON_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    poisson_check = .false.
    return
  end if

  poisson_check = .true.

  return
end

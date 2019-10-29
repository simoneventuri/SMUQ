function semicircular_check ( a, b )

!*****************************************************************************80
!
!! SEMICIRCULAR_CHECK checks the parameters of the Semicircular CDF.
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
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, logical SEMICIRCULAR_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical semicircular_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SEMICIRCULAR_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.0'
    semicircular_check = .false.
    return
  end if

  semicircular_check = .true.

  return
end

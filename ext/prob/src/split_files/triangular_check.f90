function triangular_check ( a, b )

!*****************************************************************************80
!
!! TRIANGULAR_CHECK checks the parameters of the Triangular CDF.
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
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, logical TRIANGULAR_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical triangular_check

  if ( b <= a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TRIANGULAR_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= A.'
    triangular_check = .false.
    return
  end if

  triangular_check = .true.

  return
end

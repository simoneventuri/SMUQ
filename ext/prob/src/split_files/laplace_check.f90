function laplace_check ( a, b )

!*****************************************************************************80
!
!! LAPLACE_CHECK checks the parameters of the Laplace PDF.
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
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical LAPLACE_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical laplace_check

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LAPLACE_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    laplace_check = .false.
    return
  end if

  laplace_check = .true.

  return
end

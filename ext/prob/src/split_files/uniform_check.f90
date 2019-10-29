function uniform_check ( a, b )

!*****************************************************************************80
!
!! UNIFORM_CHECK checks the parameters of the Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
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
!    Output, logical UNIFORM_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical uniform_check

  if ( b <= a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'UNIFORM_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= A.'
    uniform_check = .false.
    return
  end if

  uniform_check = .true.

  return
end

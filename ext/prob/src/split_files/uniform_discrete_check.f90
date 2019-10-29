function uniform_discrete_check ( a, b )

!*****************************************************************************80
!
!! UNIFORM_DISCRETE_CHECK checks the parameters of the Uniform discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, logical UNIFORM_DISCRETE_CHECK, is true if the parameters
!    are legal.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  logical uniform_discrete_check

  if ( b < a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'UNIFORM_DISCRETE_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B < A.'
    uniform_discrete_check = .false.
    return
  end if

  uniform_discrete_check = .true.

  return
end

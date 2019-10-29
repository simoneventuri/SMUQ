function dirichlet_check ( n, a )

!*****************************************************************************80
!
!! DIRICHLET_CHECK checks the parameters of the Dirichlet PDF.
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
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be positive.
!
!    Output, logical DIRICHLET_CHECK, is true if the parameters are legal.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  logical dirichlet_check
  integer ( kind = 4 ) i
  logical positive

  positive = .false.

  do i = 1, n

    if ( a(i) <= 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIRICHLET_CHECK - Warning!'
      write ( *, '(a)' ) '  A(I) <= 0.'
      write ( *, '(a,i8)' ) '  For I = ', i
      write ( *, '(a,g14.6)' ) '  A(I) = ', a(i)
      dirichlet_check = .false.
      return
    else if ( 0.0D+00 < a(i) ) then
      positive = .true.
    end if

  end do

  if ( .not. positive ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIRICHLET_CHECK - Warning!'
    write ( *, '(a)' ) '  All parameters are zero!'
    dirichlet_check = .false.
    return
  end if

  dirichlet_check = .true.

  return
end

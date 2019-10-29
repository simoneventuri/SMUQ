function hypergeometric_check ( n, m, l )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_CHECK checks the parameters of the Hypergeometric CDF.
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
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls in the population.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, logical HYPERGEOMETRIC_CHECK, is true if the parameters are legal.
!
  implicit none

  logical hypergeometric_check
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  if ( n < 0 .or. l < n ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HYPERGEOMETRIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  Input N is out of range.'
    hypergeometric_check = .false.
    return
  end if

  if ( m < 0 .or. l < m ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HYPERGEOMETRIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  Input M is out of range.'
    hypergeometric_check = .false.
    return
  end if

  if ( l < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HYPERGEOMETRIC_CHECK - Fatal error!'
    write ( *, '(a)' ) '  Input L is out of range.'
    hypergeometric_check = .false.
    return
  end if

  hypergeometric_check = .true.

  return
end

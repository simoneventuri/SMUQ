function empirical_discrete_check ( a, b, c )

!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_CHECK checks the parameters of the Empirical Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, logical EMPIRICAL_DISCRETE_CHECK, is true if the parameters
!    are legal.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) c(a)
  logical empirical_discrete_check
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  if ( a <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Warning!'
    write ( *, '(a)' ) '  A must be positive.'
    write ( *, '(a,i12)' ) '  Input A = ', a
    write ( *, '(a)' ) '  A is the number of weights.'
    empirical_discrete_check = .false.
    return
  end if

  if ( any ( b(1:a) < 0.0D+00 ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Warning!'
    write ( *, '(a)' ) '  Some B(*) < 0.'
    write ( *, '(a)' ) '  But all B values must be nonnegative.'
    empirical_discrete_check = .false.
    return
  end if

  if ( all ( b(1:a) == 0.0D+00 ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Warning!'
    write ( *, '(a)' ) '  All B(*) = 0.'
    write ( *, '(a)' ) '  But at least one B values must be nonzero.'
    empirical_discrete_check = .false.
    return
  end if

  do i = 1, a
    do j = i+1, a
      if ( c(i) == c(j) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Warning!'
        write ( *, '(a)' ) '  All values C must be unique.'
        write ( *, '(a)' ) '  But at least two values are identical.'
        empirical_discrete_check = .false.
        return
      end if
    end do
  end do

  do i = 1, a-1
    if ( c(i+1) < c(i) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EMPIRICAL_DISCRETE_CHECK - Warning!'
      write ( *, '(a)' ) '  The values in C must be in ascending order.'
      empirical_discrete_check = .false.
      return
    end if
  end do

  empirical_discrete_check = .true.

  return
end

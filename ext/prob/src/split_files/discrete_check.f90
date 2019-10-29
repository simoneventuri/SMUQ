function discrete_check ( a, b )

!*****************************************************************************80
!
!! DISCRETE_CHECK checks the parameters of the Discrete CDF.
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
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Output, logical DISCRETE_CHECK, is true if the parameters are legal.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) b_sum
  logical discrete_check
  integer ( kind = 4 ) j

  do j = 1, a
    if ( b(j) < 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DISCRETE_CHECK - Warning!'
      write ( *, '(a)' ) '  Negative probabilities not allowed.'
      discrete_check = .false.
      return
    end if
  end do

  b_sum = sum ( b(1:a) )

  if ( b_sum == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DISCRETE_CHECK - Warning!'
    write ( *, '(a)' ) '  Total probablity is zero.'
    discrete_check = .false.
    return
  end if

  discrete_check = .true.

  return
end
